use std::sync::atomic::{AtomicUsize, Ordering};

use dashmap::{DashMap};
use egg::*;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

#[derive(Debug, Default)]
struct RuleStats {
    times_applied: AtomicUsize,
    times_banned: AtomicUsize,
    banned_until: AtomicUsize,
}

pub struct ParallelBackoffScheduler {
    match_limit: usize,
    ban_length: usize,
    stats: DashMap<Symbol, RuleStats>,
}

impl ParallelBackoffScheduler {
    pub fn new() -> Self {
        Self {
            match_limit: 1_000,
            ban_length: 5,
            stats: DashMap::new(),
        }
    }

    fn search_rewrite_atomic<'a, L, N>(
        &self,
        iteration: usize,
        egraph: &EGraph<L, N>,
        rewrite: &'a Rewrite<L, N>,
    ) -> Vec<SearchMatches<'a, L>>
    where
        L: Language + Send + Sync,
        N: Analysis<L> + Send + Sync,
        <L as Language>::Discriminant: Sync,
        <N as Analysis<L>>::Data: Sync,
    {
        let stats = self.stats.entry(rewrite.name).or_default();

        let banned_until = stats.banned_until.load(Ordering::Acquire);
        if iteration < banned_until {
            return vec![];
        }

        let times_banned = stats.times_banned.load(Ordering::Acquire);
        let thresold = self.match_limit.checked_shl(times_banned as u32).unwrap();
        let matches = rewrite.search_with_limit(egraph, thresold.saturating_add(1));
        let total_len: usize = matches.iter().map(|m| m.substs.len()).sum();
        if total_len > thresold {
            let ban_length = self.ban_length << times_banned;
            stats.times_banned.fetch_add(1, Ordering::AcqRel);
            stats
                .banned_until
                .store(iteration + ban_length, Ordering::Release);
            vec![]
        } else {
            stats.times_applied.fetch_add(1, Ordering::Relaxed);
            matches
        }
    }
}

impl<L, N> RewriteScheduler<L, N> for ParallelBackoffScheduler
where
    L: Language + Send + Sync,
    N: Analysis<L> + Send + Sync,
    <L as Language>::Discriminant: Sync,
    <N as Analysis<L>>::Data: Sync,
{
    fn can_stop(&mut self, iteration: usize) -> bool {
        let banned: Vec<_> = self
            .stats
            .iter()
            .filter(|s| s.banned_until.load(Ordering::Relaxed) > iteration)
            .collect();

        let min_ban = banned
            .iter()
            .map(|s| s.banned_until.load(Ordering::Relaxed))
            .min();

        if let Some(min_ban) = min_ban {
            assert!(min_ban >= iteration);
            let delta = min_ban - iteration;

            for s in banned {
                s.banned_until.fetch_sub(delta, Ordering::Relaxed);
            }
            false
        } else {
            true
        }
    }

    fn search_rewrite<'a>(
        &mut self,
        iteration: usize,
        egraph: &EGraph<L, N>,
        rewrite: &'a Rewrite<L, N>,
    ) -> Vec<SearchMatches<'a, L>> {
        self.search_rewrite_atomic(iteration, egraph, rewrite)
    }

    fn search_rewrites<'a>(
        &mut self,
        iteration: usize,
        egraph: &EGraph<L, N>,
        rewrites: &[&'a Rewrite<L, N>],
        limits: &RunnerLimits,
    ) -> RunnerResult<Vec<Vec<SearchMatches<'a, L>>>> {
        let matches = rewrites
            .par_iter()
            .map(|rw| self.search_rewrite_atomic(iteration, egraph, rw))
            .collect();
        limits.check_limits(iteration, egraph)?;
        Ok(matches)
    }
}
