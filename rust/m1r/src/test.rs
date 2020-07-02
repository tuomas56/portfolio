/*counts[0] = 10;
        counts[1] = 30;
        counts[2] = 0;
        counts[3] = 0;
        counts[4] = 10;
        counts[5] = 10;*/
        /*unsafe {
            initialize_propensities.call();
        }
        let mut rng = rand::thread_rng();
        let mut time = 0.0;
        while time < 10.0 {
            let alpha0: f32 = 1.0;//propensities.iter().cloned().sum();
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
            time += tau;
        }*/

    let init_function = module.add_function("initialize_propensities", void_type.fn_type(&[], false), None);
    let entry_block = context.append_basic_block(&init_function, "entry");

    for (_, idx, expr) in expr_pairs.clone() {
        let res = build_expr(expr, idx, rate_array, &count_idx, count_array, &volumes, &builder, &f32_type, &i64_type);
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