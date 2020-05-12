open Llvm.PassManager
open Llvm_scalar_opts
open Llvm_ipo


let optimize llvm_module =
        let module_pm = create () in
        let optimizations = [
                add_basic_alias_analysis;
                add_cfg_simplification;
                add_function_attrs;
                add_constant_merge;
                add_global_dce;
                add_cfg_simplification;
                add_dead_store_elimination;
                add_reassociation;
                add_constant_propagation;
                add_early_cse;
                add_ipc_propagation;
                add_dead_arg_elimination;
                add_argument_promotion;
                add_cfg_simplification;
                add_function_inlining;
                add_global_optimizer;
                add_ipsccp;
                add_global_dce;
                add_cfg_simplification;
                add_memory_to_register_promotion;
                add_instruction_combination;
                add_gvn;
                add_licm;
                add_ind_var_simplification;
                add_loop_unroll;
                add_loop_deletion;
                add_cfg_simplification;

        ] in
        List.iter ( (|>) module_pm) optimizations;
        ignore(run_module llvm_module module_pm)

