let processCallee = (~def, ~loc, callee) =>
  switch (callee) {
  | Path.Pident(id) =>
    let id = Ident.name(id);
    switch (Il.findDef(~id)) {
    | Some(defCallee) => def |> Il.Def.emit(~instr=Il.Call(defCallee.id))
    | None =>
      Log_.info(~count=false, ~loc, ~name="Noalloc", (ppf, ()) =>
        Format.fprintf(ppf, "Callee not recognized: %s", id)
      );
      assert(false);
    };
  | _ =>
    switch (callee |> Path.name) {
    | "Pervasives.+"
    | "Stdlib.+" => def |> Il.Def.emit(~instr=Il.I32Add)
    | name =>
      Log_.info(~count=false, ~loc, ~name="Noalloc", (ppf, ()) =>
        Format.fprintf(ppf, "Callee not recognized: %s", name)
      );
      assert(false);
    }
  };

type instKind =
  | Param
  | Decl
  | Set;

let rec processTyp = (~def: Il.Def.t, typ: Types.type_expr) =>
  switch (typ.desc) {
  | Ttuple(ts) =>
    let scopes = ts |> List.map(processTyp(~def));
    Il.Env.Tuple(scopes);
  | _ =>
    let offset = def.nextOffset;
    def.nextOffset = offset + 1;
    Il.Env.Local(offset);
  };

let rec processScope = (~def: Il.Def.t, ~instrKind, ~scope: Il.Env.scope) => {
  switch (scope) {
  | Tuple(scopes) =>
    scopes |> List.iter(s => {processScope(~def, ~instrKind, ~scope=s)})
  | Local(offset) =>
    let instr =
      switch (instrKind) {
      | Param => Il.Param(offset)
      | Decl => Il.LocalDecl(offset)
      | Set => Il.LocalSet(offset)
      };
    def |> Il.Def.emit(~instr);
  };
};

let rec processFunPat = (~def, ~env, pat: Typedtree.pattern) =>
  switch (pat.pat_desc) {
  | Tpat_var(id, _)
  | Tpat_alias({pat_desc: Tpat_any}, id, _) =>
    let scope = pat.pat_type |> processTyp(~def);
    processScope(~def, ~instrKind=Param, ~scope);
    let newEnv =
      env |> Il.Env.addFunctionParameter(~id=id |> Ident.name, ~scope);
    (newEnv, scope);

  | Tpat_tuple(pats) =>
    let (newEnv, scopes) =
      pats
      |> List.fold_left(
           ((e, scopes), p) => {
             let (newEnv, scope) = p |> processFunPat(~def, ~env=e);
             (newEnv, [scope, ...scopes]);
           },
           (env, []),
         );
    (newEnv, Il.Env.Tuple(scopes));

  | _ =>
    Log_.info(~count=false, ~loc=pat.pat_loc, ~name="Noalloc", (ppf, ()) =>
      Format.fprintf(ppf, "Argument pattern not supported")
    );
    assert(false);
  };

let rec processFunDef = (~def, ~env, ~params, expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_function({
      arg_label: Nolabel,
      param,
      cases: [{c_lhs, c_guard: None, c_rhs}],
      partial: Total,
    }) =>
    let (newEnv, typ) = c_lhs |> processFunPat(~def, ~env);
    c_rhs
    |> processFunDef(~def, ~env=newEnv, ~params=[(param, typ), ...params]);

  | _ => (env, expr, params)
  };

let processConst = (~def, ~loc, const: Asttypes.constant) =>
  switch (const) {
  | Const_int(n) =>
    def |> Il.Def.emit(~instr=Il.Const(Il.I32(n |> Int32.of_int)))
  | Const_float(s) =>
    let sWithDecimal = s.[String.length(s) - 1] == '.' ? s ++ "0" : s;
    def |> Il.Def.emit(~instr=Il.Const(Il.F64(sWithDecimal)));
  | _ =>
    Log_.info(~count=false, ~loc, ~name="Noalloc", (ppf, ()) =>
      Format.fprintf(ppf, "Constant not supported")
    );
    assert(false);
  };

let rec processLocalBinding =
        (~def: Il.Def.t, ~env, ~expr, ~pat: Typedtree.pattern, ~scope) =>
  switch (pat.pat_desc) {
  | Tpat_var(id, _) =>
    expr |> processExpr(~def, ~env);
    env |> Il.Env.addFunctionParameter(~id=id |> Ident.name, ~scope);

  | Tpat_tuple(pats) => assert(false)
  // pats
  // |> List.fold_right((e, p) => processLocalBinding(~def, ~env=e), env)

  | _ => assert(false)
  }

and processExpr = (~def, ~env, expr: Typedtree.expression) =>
  switch (expr.exp_desc) {
  | Texp_constant(const) => const |> processConst(~def, ~loc=expr.exp_loc)

  | Texp_ident(id, _, _) =>
    let id = Path.name(id);
    let rec emitScope = (scope: Il.Env.scope) =>
      switch (scope) {
      | Local(offset) => def |> Il.Def.emit(~instr=Il.LocalGet(offset))
      | Tuple(scopes) => scopes |> List.iter(emitScope)
      };
    switch (env |> Il.Env.find(~id)) {
    | Some(scope) => emitScope(scope)

    | None =>
      Log_.info(~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
        Format.fprintf(ppf, "Id not found: %s", id)
      );

      assert(false);
    };

  | Texp_apply(
      {exp_desc: Texp_ident(callee, _, _), exp_loc: callee_loc},
      args,
    ) =>
    args
    |> List.iter(((argLabel: Asttypes.arg_label, argOpt)) =>
         switch (argLabel, argOpt) {
         | (Nolabel, Some(arg)) => arg |> processExpr(~def, ~env)
         | _ =>
           Log_.info(
             ~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
             Format.fprintf(ppf, "Argument not supported")
           )
         }
       );
    callee |> processCallee(~def, ~loc=callee_loc);

  | Texp_function(_) =>
    let (env, body, params) = expr |> processFunDef(~def, ~env, ~params=[]);
    if (params == []) {
      Log_.info(~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
        Format.fprintf(ppf, "Cannot decode function parameters")
      );
      assert(false);
    };
    def.params = params;
    body |> processExpr(~def, ~env);

  | Texp_tuple(l) => l |> List.iter(processExpr(~def, ~env))

  | Texp_let(Nonrecursive, [vb], inExpr) =>
    let scope = vb.vb_expr.exp_type |> processTyp(~def);
    processScope(~def, ~instrKind=Decl, ~scope);
    let newEnv =
      processLocalBinding(
        ~def: Il.Def.t,
        ~env,
        ~expr=vb.vb_expr,
        ~pat=vb.vb_pat,
        ~scope,
      );
    processScope(~def, ~instrKind=Set, ~scope);
    inExpr |> processExpr(~def, ~env=newEnv);

  | _ =>
    Log_.info(~count=false, ~loc=expr.exp_loc, ~name="Noalloc", (ppf, ()) =>
      Format.fprintf(ppf, "Expression not supported")
    );
    assert(false);
  };

let processValueBinding = (~loc, ~id, ~expr: Typedtree.expression) => {
  Log_.item("no-alloc binding for %s@.", id |> Ident.name);
  let def = Il.createDef(~loc, ~id);
  let env = Il.Env.create();
  expr |> processExpr(~def, ~env);
};

let collectValueBinding = (super, self, vb: Typedtree.value_binding) => {
  switch (vb.vb_pat.pat_desc) {
  | Tpat_var(id, _)
      when vb.vb_attributes |> Annotation.hasAttribute((==)("noalloc")) =>
    processValueBinding(~loc=vb.vb_loc, ~id, ~expr=vb.Typedtree.vb_expr)
  | _ => ()
  };
  let r = super.Tast_mapper.value_binding(self, vb);
  r;
};

let traverseStructure = {
  /* Tast_mapper */
  let super = Tast_mapper.default;

  let value_binding = (self, vb) => vb |> collectValueBinding(super, self);
  Tast_mapper.{...super, value_binding};
};

let processCmt = (cmt_infos: Cmt_format.cmt_infos) =>
  switch (cmt_infos.cmt_annots) {
  | Interface(_) => ()
  | Implementation(structure) =>
    structure |> traverseStructure.structure(traverseStructure) |> ignore
  | _ => ()
  };

let reportResults = (~ppf) => Il.dumpDefs(~ppf);
