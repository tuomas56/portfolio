#![feature(test)]
#![recursion_limit="1024"]

extern crate rand;
extern crate test;
extern crate petgraph;
extern crate inkwell;

use rand::Rng;
use petgraph::{Graph, dot::Dot, graph::{DefaultIx, NodeIndex}, Direction};
use std::{fmt, collections::{HashSet, HashMap}};
use std::time;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Module};
use inkwell::values::{FloatValue, GlobalValue};
use inkwell::types::{FloatType, IntType};
use inkwell::execution_engine::{JitFunction};
use inkwell::{OptimizationLevel};
use inkwell::passes::{PassManager, PassManagerBuilder};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Species(&'static str);

impl fmt::Debug for Species {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq)]
struct Reaction {
    lhs: Vec<(usize, Species)>,
    rhs: Vec<(usize, Species)>,
    rate: f32
}

impl fmt::Debug for Reaction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.lhs.is_empty() {
            write!(f, "{{}}")?;
        } else {
            let last = self.lhs.len() - 1;
            for (i, &(count, Species(reactant))) in self.lhs.iter().enumerate() {
                if count == 1 {
                    write!(f, "{}", reactant)?;
                } else {
                    write!(f, "{}{}", count, reactant)?;
                }
                if i < last {
                    write!(f, " + ")?;
                }
            }
        }
        write!(f, " -> ")?;
        if self.rhs.is_empty() {
            write!(f, "{{}}")?;
        } else {
            let last = self.rhs.len() - 1;
            for (i, &(count, Species(reactant))) in self.rhs.iter().enumerate() {
                if count == 1 {
                    write!(f, "{}", reactant)?;
                } else {
                    write!(f, "{}{}", count, reactant)?;
                }
                if i < last {
                    write!(f, " + ")?;
                }
            }
        }
        write!(f, " ({})", self.rate)
    }
}

// These two macros parse reactions written in regular notation, like so:
// A + 3*B => C (k), where k is a literal representing the rate. 
// Please don't ask me how they work, it's not nice.
macro_rules! reactions {
    { @parse ($($stack:tt)+) -> { $($cf:literal *)?$xf:ident $(+ $($c:literal *)?$x:ident)* => $($df:literal *)?$yf:ident $(+ $($d:literal *)?$y:ident)* ($k:literal); $($rest:tt)* } } => {
        reactions! { @parse ($($stack)+, Reaction { 
                lhs: vec![(switch_lit!($($cf)?), Species(stringify!($xf))), $((switch_lit!($($c)?), Species(stringify!($x)))),*],
                rhs: vec![(switch_lit!($($df)?), Species(stringify!($yf))), $((switch_lit!($($d)?), Species(stringify!($y)))),*],
                rate: $k
            }) -> { $($rest)* }
        }
    };
    { @parse ($($stack:tt)+) -> { {} => $($df:literal *)?$yf:ident $(+ $($d:literal *)?$y:ident)* ($k:literal); $($rest:tt)* } } => {
        reactions! { @parse ($($stack)+, Reaction { 
                lhs: Vec::new(),
                rhs: vec![(switch_lit!($($df)?), Species(stringify!($yf))), $((switch_lit!($($d)?), Species(stringify!($y)))),*],
                rate: $k
            }) -> { $($rest)* }
        }
    };
    { @parse ($($stack:tt)+) -> { $($cf:literal *)?$xf:ident $(+ $($c:literal *)?$x:ident)* => {} ($k:literal); $($rest:tt)* } } => {
        reactions! { @parse ($($stack)+, Reaction { 
                lhs: vec![(switch_lit!($($cf)?), Species(stringify!($xf))), $((switch_lit!($($c)?), Species(stringify!($x)))),*],
                rhs: Vec::new(),
                rate: $k
            }) -> { $($rest)* }
        }
    };
    { @parse ($($stack:tt)+) -> {} } => {
        vec![$($stack)+]
    };
    { volume: $volume:literal species: $($sn:ident: $si:literal),+ reactions: $($cf:literal *)?$xf:ident $(+ $($c:literal *)?$x:ident)* => $($df:literal *)?$yf:ident $(+ $($d:literal *)?$y:ident)* ($k:literal); $($rest:tt)* } => {
        ($volume, {
            let mut map = HashMap::new();
            $(map.insert(Species(stringify!($sn)), $si);)+
            map
        }, reactions! { @parse (Reaction { 
                lhs: vec![(switch_lit!($($cf)?), Species(stringify!($xf))), $((switch_lit!($($c)?), Species(stringify!($x)))),*],
                rhs: vec![(switch_lit!($($df)?), Species(stringify!($yf))), $((switch_lit!($($d)?), Species(stringify!($y)))),*],
                rate: $k 
            }) -> { $($rest)* }
        })
    };
    { volume: $volume:literal species: $($sn:ident: $si:literal),+ reactions: $($cf:literal *)?$xf:ident $(+ $($c:literal *)?$x:ident)* => {} ($k:literal); $($rest:tt)* } => {
        ($volume, {
            let mut map = HashMap::new();
            $(map.insert(Species(stringify!($sn)), $si);)+
            map
        }, reactions! { @parse (Reaction { 
                lhs: vec![(switch_lit!($($cf)?), Species(stringify!($xf))), $((switch_lit!($($c)?), Species(stringify!($x)))),*],
                rhs: Vec::new(),
                rate: $k 
            }) -> { $($rest)* }
        })
    };
    { volume: $volume:literal species: $($sn:ident: $si:literal),+ reactions: {} => $($df:literal *)?$yf:ident $(+ $($d:literal *)?$y:ident)* ($k:literal); $($rest:tt)* } => {
        ($volume, {
            let mut map = HashMap::new();
            $(map.insert(Species(stringify!($sn)), $si);)+
            map
        }, reactions! { @parse (Reaction { 
                lhs: Vec::new(),
                rhs: vec![(switch_lit!($($df)?), Species(stringify!($yf))), $((switch_lit!($($d)?), Species(stringify!($y)))),*],
                rate: $k 
            }) -> { $($rest)* }
        })
    };
}

macro_rules! switch_lit {
    { } => { 1 };
    { $c:literal } => { $c };
}

// This data structure represents the possible
// propensity expressions. (Volume(k) represents volume^k)
#[derive(Debug, Clone)]
enum Expr {
    Mul(Vec<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Count(Species),
    Rate,
    Volume(isize),
    Constant(f32)
}

//Calculate one factor of the propensity expression.
fn one_propensity(k: (usize, Species)) -> Vec<Expr> {
    let mut muls = Vec::new();
    let (mut c, s) = k;
    let mut a = 1;
    muls.push(Expr::Count(s));
    c -= 1;
    while c > 0 {
        // For each copy of this species, we multiply by the remaining molecules
        muls.push(Expr::Sub(Box::new(Expr::Count(s)), Box::new(Expr::Constant(a as f32))));
        a += 1;
        c -= 1;
    }
    muls
}

fn propensity_expr(r: &Reaction) -> Expr {
    // Calculate the order by summing stochiometric coefficients.
    let order: usize = r.lhs.iter().map(|(c, _)| *c).sum();
    if order == 0 {
        // Zero order (i.e production) reactions:
        Expr::Mul(vec![Expr::Rate, Expr::Volume(1)])
    } else if order == 1 {
        // Order 1 (i.e conversion) reactions:
        Expr::Mul(vec![Expr::Rate, Expr::Count(r.lhs.first().unwrap().1)])
    } else {
        // Every other order is handled by splitting it into different species
        let mut factors: Vec<Expr> = r.lhs.iter().cloned().map(one_propensity).flatten().collect();
        // Then multiplying by the rate,
        factors.insert(0, Expr::Rate);
        // And the appropriate volume factor.
        factors.push(Expr::Volume(1 - (order as isize)));
        Expr::Mul(factors)
    }
}

// Construct a dependency graph from the list of reactions to show which
// propensities need to be updated when a given reaction occurs.
fn dependency_graph(reactions: Vec<Reaction>) -> (Graph<Reaction, Species>, Vec<(Reaction, NodeIndex<DefaultIx>)>) {
    let mut graph = Graph::new();
    let mut indices = Vec::new();
    let mut map = Vec::new();

    // Make a node in the graph for every reaction.
    for i in 0..reactions.len() {
        let idx = graph.add_node(reactions[i].clone());
        map.push((reactions[i].clone(), idx));
        indices.push(idx);
    }

    for i in 0..reactions.len() {
        for j in 0..reactions.len() {
            // For each pair of reactions,
            if i == j {
                continue;
            }

            for &(_, product) in &reactions[i].rhs {
                for &(_, reactant) in &reactions[j].lhs {
                    if product == reactant {
                        // If one of the products of the first is a reactant of the second,
                        if !graph.contains_edge(indices[i], indices[j]) {
                            // Add an edge from the first to the second
                            graph.add_edge(indices[i], indices[j], product);
                        }
                        break;
                    }
                }
            }

            for &(_, reactant1) in &reactions[i].lhs {
                for &(_, reactant) in &reactions[j].lhs {
                    if reactant1 == reactant {
                        // If one of the products of the first is a reactant of the second,
                        if !graph.contains_edge(indices[i], indices[j]) {
                            // Add an edge from the first to the second
                            graph.add_edge(indices[i], indices[j], reactant1);
                        }
                        break;
                    }
                }
            }
        }
    }

    (graph, map)
}

fn get_used_volumes(expr: &Expr, set: &mut HashSet<isize>) {
    match expr {
        Expr::Volume(i) => {
            set.insert(*i);
        },
        Expr::Sub(a, b) => {
            get_used_volumes(a, set);
            get_used_volumes(b, set);
        },
        Expr::Mul(exprs) => {
            for expr in exprs {
                get_used_volumes(expr, set);
            }
        },
        _ => ()
    }
}

fn get_species(reaction: &Reaction, set: &mut HashSet<Species>) {
    set.extend(reaction.lhs.iter().map(|(_, r)| r));
    set.extend(reaction.rhs.iter().map(|(_, r)| r));
}

fn state_update_vector(reaction: &Reaction, count_idx: &HashMap<Species, usize>) -> HashMap<usize, i64> {
    let mut map = HashMap::new();
    for (c, r) in &reaction.lhs {
        map.insert(count_idx[&r], -(*c as i64));
    }
    for (c, r) in &reaction.rhs {
        if map.contains_key(&count_idx[&r]) {
            *map.get_mut(&count_idx[&r]).unwrap() += *c as i64;
        } else {
            map.insert(count_idx[&r], *c as i64);
        }
    }
    map
}

fn build_expr(
    expr: Expr, volume: f32,
    rate_idx: usize, rate_array: GlobalValue,
    count_idx: &HashMap<Species, usize>, count_array: GlobalValue,
    builder: &Builder, f32_type: &FloatType, i64_type: &IntType
) -> FloatValue {
    match expr {
        Expr::Mul(mut exprs) => {
            let mut prev = build_expr(
                exprs.remove(0), volume, rate_idx, rate_array, 
                count_idx, count_array, builder, f32_type, i64_type
            );
            for val in exprs {
                let next = build_expr(
                    val, volume, rate_idx, rate_array, count_idx,
                    count_array, builder, f32_type, i64_type
                );
                prev = builder.build_float_mul(prev, next, "mul");
            }

            prev
        },
        Expr::Sub(a, b) => {
            let a = build_expr(
                *a, volume, rate_idx, rate_array, count_idx,
                count_array, builder, f32_type, i64_type
            );
            let b = build_expr(
                *b, volume, rate_idx, rate_array, count_idx,
                count_array, builder, f32_type, i64_type
            );
            builder.build_float_sub(a, b, "sub")
        },
        Expr::Count(s) => {
            let ptr = unsafe {
                builder.build_in_bounds_gep(
                    count_array.as_pointer_value(), &[i64_type.const_int(0, false),
                    i64_type.const_int(count_idx[&s] as u64, false)], "count_ptr"
                )
            };
            let ival = builder.build_load(ptr, "count_int").into_int_value();
            builder.build_unsigned_int_to_float(ival, *f32_type, "count")
        },
        Expr::Rate => {
            let ptr = unsafe {
                builder.build_in_bounds_gep(
                    rate_array.as_pointer_value(),
                    &[i64_type.const_int(0, false), i64_type.const_int(rate_idx as u64, false)],
                    "rate_ptr"
                )
            };
            builder.build_load(ptr, "rate").into_float_value()
        },
        Expr::Volume(i) => f32_type.const_float(volume.powi(i as i32) as f64),
        Expr::Constant(c) => f32_type.const_float(c as f64)
    }
}

fn make_update_module(reactions: Vec<Reaction>, volume: f32) -> (Module, GlobalValue, GlobalValue, GlobalValue, usize, HashMap<Species, usize>) {
    let (dep_graph, reaction_idxs) = dependency_graph(reactions.clone());

    let context = Context::create();
    let module = context.create_module("reaction");
    let builder = context.create_builder();
    let f32_type = context.f32_type();
    let i64_type = context.i64_type();
    let void_type = context.void_type();

    let mut used_volumes = HashSet::new();
    let mut species = HashSet::new();
    let mut expr_pairs: Vec<(NodeIndex<DefaultIx>, usize, Expr)> = Vec::new();
    for (idx, reaction) in reactions.iter().enumerate() {
        let expr = propensity_expr(reaction);
        println!("{:?} {:?}", reaction, expr);
        get_used_volumes(&expr, &mut used_volumes);
        get_species(reaction, &mut species);
        let (_, nidx) = reaction_idxs.iter().find(|(r, _)| r == reaction).unwrap();
        expr_pairs.push((*nidx, idx, expr));
    }
    let splen = species.len();

    /*let mut volumes = HashMap::new();
    for i in &used_volumes {
        let glob = module.add_global(f32_type, None, &format!("volume_{}", i));
        glob.set_initializer(&f32_type.const_float(0.0));
        volumes.insert(*i, glob);
    }*/

    let mut species = species.drain().collect::<Vec<_>>();
    species.sort();
    let count_idx = species.drain(..).enumerate().map(|(i, c)| (c, i)).collect::<HashMap<_, _>>();

    let mut state_vectors = HashMap::new();
    for (idx, reaction) in reactions.iter().enumerate() {
        state_vectors.insert(idx, state_update_vector(reaction, &count_idx));
    }

    let rate_array_type = f32_type.array_type(reactions.len() as u32);
    let rate_array = module.add_global(rate_array_type, None, "rates");

    let count_array_type = i64_type.array_type(splen as u32);
    let count_array = module.add_global(count_array_type, None, "counts");

    let propensity_array_type = f32_type.array_type(reactions.len() as u32);
    let propensity_array = module.add_global(propensity_array_type, None, "propensities");

    let update_table_type = void_type.fn_type(&[i64_type.into(), f32_type.into()], false);
    let update_table = module.add_function("update_table", update_table_type, None);

    let fn_type = void_type.fn_type(&[i64_type.into()], false);
    let function = module.add_function("update_propensity", fn_type, None);
    let entry_block = context.append_basic_block(&function, "entry");

    let mut idx_blocks = HashMap::new();
    let mut nidx_idx = HashMap::new();
    for (nidx, idx, _) in &expr_pairs {
        let basic_block = context.append_basic_block(&function, &format!("reaction_{}", idx));
        idx_blocks.insert(*idx, basic_block);
        nidx_idx.insert(*nidx, *idx);
    }

    for (nidx, idx, expr) in expr_pairs.clone() {
        builder.position_at_end(&idx_blocks[&idx]);
        for (i, c) in state_vectors[&idx].iter() {
            if *c != 0 {
                let ptr = unsafe {
                    builder.build_in_bounds_gep(
                        count_array.as_pointer_value(), 
                        &[i64_type.const_int(0, false), i64_type.const_int(*i as u64, false)],
                        "count_update_ptr"
                    )
                };
                let old = builder.build_load(ptr, "count_old").into_int_value();
                let new = if *c > 0 {
                    builder.build_int_add(old, i64_type.const_int(*c as u64, false), "count_new")
                } else {
                    builder.build_int_sub(old, i64_type.const_int((-*c) as u64, false), "count_new")
                };
                builder.build_store(ptr, new);
            }
        }
        let res = build_expr(expr, volume, idx, rate_array, &count_idx, count_array, &builder, &f32_type, &i64_type);
        let prop_ptr = unsafe {
            builder.build_in_bounds_gep(
                propensity_array.as_pointer_value(), 
                &[i64_type.const_int(0, false), i64_type.const_int(idx as u64, false)],
                "prop_ptr"
            )
        };
        builder.build_store(prop_ptr, res);
        //builder.build_call(update_table, &[i64_type.const_int(idx as u64, false).into(), delta.into()], "call_update");
        for pred in dep_graph.neighbors_directed(nidx, Direction::Outgoing) {
            let pidx = nidx_idx[&pred];
            let pexpr = expr_pairs[pidx].2.clone();
            let res = build_expr(pexpr, volume, pidx, rate_array, &count_idx, count_array, &builder, &f32_type, &i64_type);
            let prop_ptr = unsafe {
                builder.build_in_bounds_gep(
                    propensity_array.as_pointer_value(), 
                    &[i64_type.const_int(0, false), i64_type.const_int(pidx as u64, false)],
                    "prop_ptr"
                )
            };
            builder.build_store(prop_ptr, res);
            //builder.build_call(update_table, &[i64_type.const_int(pidx as u64, false).into(), delta.into()], "call_update");
        }
        builder.build_return(None);
    }

    let else_block = context.append_basic_block(&function, "out_of_bounds");
    builder.position_at_end(&else_block);
    builder.build_unreachable();

    builder.position_at_end(&entry_block);
    let rnum = function.get_first_param().unwrap().into_int_value();
    let switch_list = idx_blocks.iter().map(|(idx, blk)| (i64_type.const_int(*idx as u64, false), blk)).collect::<Vec<_>>();
    builder.build_switch(rnum, &else_block, &switch_list[..]);

    let init_function = module.add_function("initialize_propensities", void_type.fn_type(&[], false), None);
    let init_entry_block = context.append_basic_block(&init_function, "entry");
    builder.position_at_end(&init_entry_block);

    for (_, idx, expr) in expr_pairs.clone() {
        let res = build_expr(expr, volume, idx, rate_array, &count_idx, count_array, &builder, &f32_type, &i64_type);
        let prop_ptr = unsafe {
            builder.build_in_bounds_gep(
                propensity_array.as_pointer_value(), 
                &[i64_type.const_int(0, false), i64_type.const_int(idx as u64, false)],
                "prop_ptr"
            )
        };
        builder.build_store(prop_ptr, res);
    }

    builder.build_return(None);

    
    let pass_manager_builder = PassManagerBuilder::create();
    pass_manager_builder.set_optimization_level(OptimizationLevel::Aggressive);
    let pass_manager = PassManager::create(());
    pass_manager_builder.populate_module_pass_manager(&pass_manager);
    pass_manager.run_on(&module);

    (module, rate_array, count_array, propensity_array, splen, count_idx)
}

fn main() {
    dynamic_llvm();
}

fn dynamic_llvm() {
    // Using the Brusselator as an example:
    let (volume, initial_counts, reactions) = reactions! {
        volume: 10.0
        species:
            A: 100, B: 170, D: 0, E: 0, X: 100, Y: 100
        reactions:
            A => X (1.0000);
            2 * X + Y => 3 * X (1.0000);
            X + B => Y + D (1.0000);
            X => E (1.0000);
    };

    println!("{:?}", reactions);

    let num_reactions = reactions.len();

    let (dep_graph, _) = dependency_graph(reactions.clone());
    println!("{:?}", Dot::new(&dep_graph));

    let (module, rate_array, count_array, propensity_array, num_species, count_idx) = make_update_module(reactions, volume);

    println!("{}", module.print_to_string().to_string());

    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Aggressive).unwrap();

    /*let mut volumes = HashMap::new();
    for (i, g) in &glob_volumes {
        volumes.insert(*i, volume.powi(*i as i32));
        execution_engine.add_global_mapping(g, volumes.get(i).unwrap() as *const f32 as usize);
    }*/

    //println!("{:?}", volumes);

    let rates = vec![1.0f32; num_reactions];
    let mut counts = vec![0u64; num_species];
    let propensities = vec![0.0f32; num_reactions];

    execution_engine.add_global_mapping(&rate_array, rates.as_ptr() as usize);
    execution_engine.add_global_mapping(&count_array, counts.as_ptr() as usize);
    execution_engine.add_global_mapping(&propensity_array, propensities.as_ptr() as usize);

    let update_propensity: JitFunction<unsafe extern "C" fn(u64) -> ()> = unsafe {
        execution_engine.get_function("update_propensity").unwrap()
    };
    let initialize_propensities: JitFunction<unsafe extern "C" fn() -> ()> = unsafe {
        execution_engine.get_function("initialize_propensities").unwrap()
    };

    let before = time::Instant::now();
    for _ in 0..10 {
        for (s, c) in &initial_counts {
            counts[count_idx[s]] = *c;
        }
        //let mut data = Vec::new();
        //let mut times = Vec::new();
        let mut rng = rand::thread_rng();
        let mut time = 0.0;
        let mut ticks = 0;
        unsafe {
            initialize_propensities.call();
        }
        while time < 100.0 {
            //println!("{} {:?} {:?}", time, counts, propensities);
            //times.push(time);
            //data.push(counts.clone());
            let alpha0: f32 = propensities.iter().cloned().sum::<f32>();
            //println!("{} {}", alpha0, alpha01);
            if alpha0 == 0.0 { break; }
            let r = rng.gen::<f32>();
            let ir2 = 1.0/(r * r - 1.0) - 1.0 - r/2.0 - (r * r)/3.0;
            let al = 0.5 * r * ir2;
            let tau = -1.0/alpha0 * al;
            //let tau = -1.0/alpha0 * rng.gen::<f32>().ln();
            let r = rng.gen::<f32>() * alpha0;
            let mut t = 0.0;
            for i in 0..num_reactions {
                let p = propensities[i];
                if t < r && r < t + p {
                    unsafe {
                        update_propensity.call(i as u64);
                    }
                    break;
                } else {
                    t += p;
                }
            }
            //println!("{}", tau);
            //println!("{} {} {}", counts[X], counts[Y], tau);
            ticks += 1;
            time += tau;
        }
        //println!("{}", ticks);
    }
    let after = time::Instant::now();
    let dur = after - before;
    println!("{:?}", dur.as_nanos());
}

/*fn naive() {
    // Using the Brusselator as an example:
    let (volume, initial_counts, reactions) = reactions! {
        volume: 10.0
        species:
            A: 100, B: 170, D: 0, E: 0, X: 100, Y: 100
        reactions:
            A => X              (1.0);
            2*X + Y => 3 * X    (1.0);
            B + X => Y + D      (1.0);
            X => E              (1.0);
    };

    let mut counts = vec![0u64; 6];
    let mut propensities = vec!

    let before = time::Instant::now();
    for _ in 0..100000 {
        counts[A] = 100;
        counts[B] = 170;
        counts[D] = 0;
        counts[E] = 0;
        counts[X] = 100;
        counts[Y] = 100;
        let mut data = Vec::new();
        let mut times = Vec::new();
        let mut rng = rand::thread_rng();
        let mut time = 0.0;
        let mut ticks = 0;
        unsafe {
            initialize_propensities.call();
        }
        while time < 1000.0 {
            //println!("{} {:?} {:?}", time, counts, propensities);
            times.push(time);
            data.push(counts.clone());
            let alpha0: f32 = propensities.iter().cloned().sum();
            if alpha0 == 0.0 { break }
            let tau = -1.0/alpha0 * rng.gen::<f32>().ln();
            let r = rng.gen::<f32>() * alpha0;
            let mut t = 0.0;
            for i in 0..num_reactions {
                let p = propensities[i];
                if t < r && r < t + p {
                    unsafe {
                        update_propensity.call(i as u64);
                    }
                    break;
                } else {
                    t += p;
                }
            }
            //println!("{}", tau);
            //println!("{} {} {}", counts[X], counts[Y], tau);
            ticks += 1;
            time += tau;
        }
    }
    let after = time::Instant::now();
    let dur = after - before;
    println!("{:?}", dur.as_nanos());
}*/
