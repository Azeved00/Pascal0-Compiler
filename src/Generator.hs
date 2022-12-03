import DataTypes

transExp :: Exp -> Table -> Temp -> [Instr]
transExp (Num n) tab dest = [MOVEI dest n]

transExp (Id s) tab dest = [MOVE dest t]
                         where (Just t) = Map.lookup s tab

transExp (BinOp op e1 e2) tab dest = code1 ++ code2 ++ [OPER op dest t1 t2]
                                   where t1 = newTemp()
                                         t2 = newTemp()
                                         code1 = transExp e1 tab t1
                                         code2 = transExp e2 tab t2
transExp (Bool b) tab dest
      | b = [MOVEI dest 1]
      | otherwise = [MOVEI dest 0]

transExp (RelOp op e1 e2) tab dest = [MOVEI dest 0] ++ code
                                     ++ [LABEL l1, MOVEI dest 1] ++ [Label l2]
                                   where l1 = newLabel()
                                         l2 = newLabel()
                                         cond = RelOp op e1 e2
                                         code = transCond cond l1 l2 tab

transExp (UnOp NOT expr) tab dest = [MOVEI dest 0] ++ code
                                    ++ [LABEL l1, MOVEI dest 1] ++ [Label l2]
                                  where l1 = newLabel()
                                        l2 = newLabel()
                                        cond = UnOp NOT expr
                                        code = transCond cond l1 l2 tab

transExp (Func id expr) tab dest = code ++ [CALL dest id tl]
                                 where (code, tl) = transExps expr tab



transExps :: Exp -> Table -> ([Instr], [Temp])
transExps (CompoundExp (e1:e2)) tab = (code, temp)
                                    where t1 = newTemp()
                                          code1 = transExp e1 tab temp
                                          (code2, t2) = transExps e2 tab
                                          code = code1 ++ code2
                                          temp = t1 : t2

transExps expr tab = (transExp expr tab temp, [temp])
                   where temp = newTemp()



transStm :: Stm -> Table -> Instr
transStm (AssignStm (Id s) e) tab = transExp e tab dest
                                  where (Just dest) = Map.lookup s tab

transStm (CompoundStm []) tab = []
transStm (CompoundStm (s1:s2)) tab = transStm s1 tab ++ transStm s2 tab

transStm (IfStm expr stm) = codec ++ [LABEL lt] ++ codet ++ [LABEL cont]
                          where lt = newLabel()
                                cont = newLabel()
                                codec = transCond expr lt cont tab
                                codet = transStm stm tab

transStm (IfElseStm expr s1 s2) tab = code1 ++ [LABEL l1] ++ code2 ++ [LABEL l3]
                                      ++ [LABEL l2] ++ code3 ++ [LABEL l3]
                                    where l1 = newLabel()
                                          l2 = newLabel()
                                          l3 = newLabel()
                                          code1 = transCond expr l1 l2 tab
                                          code2 = transStm s1 tab
                                          code3 = transStm s2 tab

transStm (WhileStm expr stm) = [LABEL l1] ++ code1 ++ [LABEL l2] ++ code2
                               ++ [JUMP l1, LABEL l3]
                             where l1 = newLabel()
                                   l2 = newLabel()
                                   l3 = newLabel()
                                   code1 = transCond expr l1 l2 tab
                                   code2 = transStm stm tab



transCond :: Exp -> Label -> Label -> Table
transCond (BinOp And c1 c2) lt lf tab = code1 ++ [Label l2] ++ code2
                                where l2 = newLabel()
                                      code1 = transCond c1 l2 lf tab
                                      code2 = transCond c2 lt lf tab

transCond (BinOp Or c1 c2) lt lf tab = code1 ++ [Label l2] ++ code2
                                where l2 = newLabel()
                                      code1 = transCond c1 lt l2 tab
                                      code2 = transCond c2 lt lf tab

transCond (BinOp relop e1 e2) lt lf tab
      = code1 ++ code2 ++ [COND (BinOp relop e1 e2) lt lf]
      where t1 = newTemp()
            t2 = newTemp()
            code1 = transExp e1 tab t1
            code2 = transExp e2 tab t2

transCond (Bool True) lt _ _ = [JUMP lt]

transCond (Bool False) _ lf _ = [JUMP lf]

transCond (UnOp Not cond) lt lf tab = transCond cond lf lt tab

transCond expr lt lf tab = code ++ [COND (BinOp DIFF t 0) lt lf]
                         where t = newTemp()
                               code = transExp expr tab t
