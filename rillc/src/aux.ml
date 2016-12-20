type 'ty t = {
    ta_type           : 'ty;
    ta_vcat           : Value_category.t;
    ta_lt             : Lifetime.t;
    ta_ml             : Meta_level.t;
    ta_loc            : Loc.t;
  }

let ty aux =
  aux.ta_type

let loc aux =
  aux.ta_loc

let ml aux =
  aux.ta_ml

let lt aux =
  aux.ta_lt

let make ~ty ~vcat ~lt ~ml ~loc =
  {
    ta_type = ty;
    ta_vcat = vcat;
    ta_lt = lt;
    ta_ml = ml;
    ta_loc = loc;
  }
