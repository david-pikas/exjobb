#![allow(dead_code)]
#![allow(unused_macros)]
use std::collections::hash_map;
use std::mem;
use std::{collections::HashMap, rc::Rc};
use crate::semantics::{Refs, Variable, LtRefs, Lifetime, Path};
use crate::context::Context;


type LtLocation = LtRefs;

pub struct LtReset {
    location: LtLocation,
    original: (bool, Vec<Lifetime>)
}
pub struct LtFinal {
    location: LtLocation,
    fin: (bool, Vec<Lifetime>)
}

type VarLocation = Rc<Variable>;

pub struct VarReset {
    location: VarLocation,
    original: Refs
}

pub struct VarFinal {
    location: VarLocation,
    fin: Refs
}

// TODO: handle scoping
pub struct Branches {
    lt_reset: HashMap<Lifetime, LtReset>,
    var_reset: HashMap<Path, VarReset>,
    branches: Vec<Branch>
}

impl Branches {
    pub fn new() -> Self { 
        Self {
            lt_reset: HashMap::new(),
            var_reset: HashMap::new(),
            branches: vec![]
        }
    }
    pub fn new_branch(&mut self) {
        let lts =
            mem::replace(&mut self.lt_reset, HashMap::new())
                .into_iter().map(|(name, lt_reset)| {
                let aliveness = lt_reset.location.0.replace(lt_reset.original.0);
                let refs      = lt_reset.location.1.replace(lt_reset.original.1);
                let fin = LtFinal {
                    fin: (aliveness, refs),
                    location: lt_reset.location,
                };
                (name, fin)
            }).collect::<HashMap<_,_>>();
        let vars =
            mem::replace(&mut self.var_reset, HashMap::new())
                .into_iter().map(|(name, var_reset)| {
                let fin = VarFinal {
                    fin: var_reset.location.refs.replace(var_reset.original),
                    location: var_reset.location,
                };
                (name, fin)
            }).collect::<HashMap<_,_>>();
        self.branches.push(Branch {
            lts, vars
        });
    }
    pub fn finish_branching(mut self) {
        if !self.lt_reset.is_empty() || !self.var_reset.is_empty() {
            self.new_branch();
        }
        let mut final_lts = HashMap::new();
        let mut final_vars = HashMap::new();
        for branch in self.branches {
            for (name, lt) in branch.lts.into_iter() {
                let entry = final_lts.entry(name);
                match entry {
                    hash_map::Entry::Occupied(_) => {
                        entry.and_modify(|old: &mut LtFinal| {
                            old.fin.0 = old.fin.0 && lt.fin.0;
                            old.fin.1.extend(lt.fin.1.clone());
                        });
                    }
                    hash_map::Entry::Vacant(_) => {
                        entry.or_insert(lt);
                    }
                }
            }
            for (name, var) in branch.vars.into_iter() {
                let entry = final_vars.entry(name);
                match entry {
                    hash_map::Entry::Occupied(_) => {
                        entry.and_modify(|old: &mut VarFinal| {
                            let old_fin = mem::replace(&mut old.fin, Refs::None);
                            old.fin = merge_refs(old_fin, var.fin);
                        });
                    }
                    hash_map::Entry::Vacant(_) => {
                        entry.or_insert(var);
                    }
                }
            }
        }
        for (_, lt) in final_lts {
            lt.location.0.replace(lt.fin.0);
            lt.location.1.replace(lt.fin.1);
        }
        for (_, var) in final_vars {
            var.location.refs.replace(var.fin);
        }
    }
}

// TODO: handle scoping
pub struct Branch {
    lts: HashMap<Lifetime, LtFinal>,
    vars: HashMap<Path, VarFinal>
}

#[macro_export]
macro_rules! branches {
    ($ctx: ident, $e: expr) => ({
        let old_branches = std::mem::replace(
            &mut $ctx.branches,
            Some(std::cell::RefCell::new(crate::branching::Branches::new()))
        );
        let result = $e;
        let finished_branches = std::mem::replace(&mut $ctx.branches, old_branches);
        finished_branches.and_then(|b| { b.into_inner().finish_branching(); None::<()> });
        result
    })
}


pub fn save_lt(ctx: &Context, lt_refs: &LtRefs, lt: Lifetime) {
    ctx.branches.as_ref().map(|bcell| {
        let mut b = bcell.borrow_mut();
        b.lt_reset.entry(lt).or_insert_with(|| {
            LtReset {
                original: (lt_refs.0.get(), lt_refs.1.borrow().clone()),
                location: lt_refs.clone(),
            }
        });
    });
}

pub fn save_var(ctx: &Context, var: &Rc<Variable>, name: Path) {
    ctx.branches.as_ref().map(|bcell| {
        let mut b = bcell.borrow_mut();
        b.var_reset.entry(name).or_insert_with(|| {
            VarReset {
                original: var.refs.borrow().clone(),
                location: var.clone(),
            }
        });
    });
}

fn merge_refs(ref1: Refs, ref2: Refs) -> Refs {
    use Refs::*;
    match (ref1, ref2) {
        (None, a) => a,
        (a, None) => a,
        (Ref(mut lts1), Ref(lts2)) => {
            lts1.extend(lts2);
            Ref(lts1)
        }
        (MutRef(lt1), MutRef(lt2)) => {
            BranchingRefs {
                mutable: vec![lt1, lt2],
                immutable: vec![]
            }
        }
        (MutRef(lt1), Ref(lts2)) => {
            BranchingRefs {
                mutable: vec![lt1],
                immutable: lts2
            }
        }
        (Ref(lts1), MutRef(lt2)) => {
            BranchingRefs {
                mutable: vec![lt2],
                immutable: lts1
            }
        }
        (MutRef(lt), BranchingRefs { mut mutable, immutable }) => {
            mutable.push(lt);
            BranchingRefs { mutable, immutable }
        }
        (BranchingRefs { mut mutable, immutable }, MutRef(lt)) => {
            mutable.push(lt);
            BranchingRefs { mutable, immutable }
        }
        (Ref(lts), BranchingRefs { mutable, mut immutable }) => {
            immutable.extend(lts);
            BranchingRefs { mutable, immutable }
        }
        (BranchingRefs { mutable, mut immutable }, Ref(lts)) => {
            immutable.extend(lts);
            BranchingRefs { mutable, immutable }
        }
        (   
            BranchingRefs { mut mutable, mut immutable },
            BranchingRefs { mutable: mutable2, immutable: immutable2 }
        ) => {
            mutable.extend(mutable2);
            immutable.extend(immutable2);
            BranchingRefs { mutable, immutable }
        }
    }
}
