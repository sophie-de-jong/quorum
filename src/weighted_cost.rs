use egg::{CostFunction, Id, Language};

use crate::math::Math;

pub fn ast_size<F>(enode: &Math, enode_weight: f64, costs: &mut F) -> f64
where
    F: FnMut(Id) -> f64,
{
    enode.fold(enode_weight, |sum, id| sum + costs(id))
}

pub fn ast_depth<F>(enode: &Math, enode_weight: f64, costs: &mut F) -> f64
where
    F: FnMut(Id) -> f64,
{
    enode_weight + enode.fold(0.0, |max: f64, id| max.max(costs(id)))
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Weights {
    operator_cost: f64,
    function_cost: f64,
    number_cost: f64,
    symbol_cost: f64,
}

impl Default for Weights {
    fn default() -> Self {
        Weights {
            operator_cost: 1.0,
            function_cost: 1.0,
            number_cost: 1.0,
            symbol_cost: 1.0,
        }
    }
}

impl Weights {
    pub fn with_operator_cost(mut self, cost: f64) -> Self {
        self.operator_cost = cost;
        self
    }

    pub fn with_function_cost(mut self, cost: f64) -> Self {
        self.function_cost = cost;
        self
    }

    pub fn with_number_cost(mut self, cost: f64) -> Self {
        self.number_cost = cost;
        self
    }

    pub fn with_symbol_cost(mut self, cost: f64) -> Self {
        self.symbol_cost = cost;
        self
    }

    pub fn weight(&self, enode: &Math) -> f64 {
        match enode {
            Math::Num(_) => self.number_cost,
            Math::Sym(_) => self.symbol_cost,
            Math::Fn(..) => self.function_cost,
            _ => self.operator_cost,
        }
    }
}

pub struct WeightedAstSize(pub Weights);
impl CostFunction<Math> for WeightedAstSize {
    type Cost = f64;

    fn cost<C>(&mut self, enode: &Math, mut costs: C) -> Self::Cost
    where
        C: FnMut(egg::Id) -> Self::Cost,
    {
        let weight = self.0.weight(enode);
        ast_size(enode, weight, &mut costs)
    }
}

pub struct WeightedAstDepth(pub Weights);
impl CostFunction<Math> for WeightedAstDepth {
    type Cost = f64;

    fn cost<C>(&mut self, enode: &Math, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let weight = self.0.weight(enode);
        ast_depth(enode, weight, &mut costs)
    }
}

pub struct WeightedAstHybrid(pub Weights);
impl CostFunction<Math> for WeightedAstHybrid {
    type Cost = f64;

    fn cost<C>(&mut self, enode: &Math, mut costs: C) -> Self::Cost
    where
        C: FnMut(Id) -> Self::Cost,
    {
        let weight = self.0.weight(enode);
        let size = ast_size(enode, weight, &mut costs);
        let depth = ast_depth(enode, weight, &mut costs);
        size + depth
    }
}
