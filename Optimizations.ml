open Llvm.PassManager
open Llvm_scalar_opts
open Llvm_ipo


let optimize llvm_module =
        let module_pm = create () in
        let optimizations = [
                add_cfg_simplification;
                add_dead_store_elimination;
                add_dead_arg_elimination;
                add_memory_to_register_promotion;
        ] in
        List.iter ( (|>) module_pm) optimizations;
        ignore(run_module llvm_module module_pm)

