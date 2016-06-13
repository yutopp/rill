module Build_info : sig
  val prefix: string
  val bin_dir: string
  val lib_dir: string
  val include_dir: string
end

val is_release: bool
val use_local_dev_lib: bool
val default_includes: string list
val default_core_lib_dir: string
val default_core_lib_name: string
val default_std_lib_dir: string
val default_std_lib_name: string
