{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE Arrows #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE TypeFamilies #-}
module Entity where

import Prelude hiding ((.), id)
import Control.Wire hiding (at,when,unless)
import Control.Lens
import Language.Haskell.TH
import Control.Arrow
import qualified Data.Map.Strict as M
import Data.Function (fix)
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Ord (comparing)
import Control.Monad.State
import Control.Monad.Fix (mfix)
import Data.Void
import Debug.Trace
import Data.Typeable
import System.IO
import Text.Regex.PCRE
import Text.Regex

data Signal c = Signal{_target::Maybe (TypeRep,Int),_payload::c}
makeLenses ''Signal

sigify :: [a] -> [Signal a]
sigify = map $ Signal Nothing

instance (Show a) => Show (Signal a) where
  show (Signal x y) = "(Signal " ++ show x ++" "++show y++")"

data Delayed a  = D a

instance (Show a) => Show (Delayed a) where
  show (D x) = "(D "++show x++")"



fromDelay :: Delayed a -> a
fromDelay (D x) = x


data family HList (l::[*])

data instance HList '[] = HNil
data instance HList (x ': xs) = x `HCons` HList xs

infixr 8 `HCons`

headH:: HList ( a  ': lst) -> a
headH (HCons x _) = x
tailH :: HList ( a  ': lst) -> HList lst
tailH (HCons _ x) = x

instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show a,Show (HList b)) => Show (HList (a ': b)) where
  show (HCons x l) = "(HCons "++show x++" "++show l++")"


data family SignalL (l::[*])

data instance SignalL '[] = SNil
data instance SignalL (x ': xs) = [Signal x] `SCons` SignalL xs

infixr 8 `SCons`

instance Show (SignalL '[]) where
  show SNil = "SNil"

instance (Show a,Show (SignalL b)) => Show (SignalL (a ': b)) where
  show (SCons x l) = "(SCons "++show x++" "++show l++")"

class EmptySig (a::[*]) where
  emptySignal :: SignalL a

instance EmptySig c => EmptySig ( a  ': c) where
  emptySignal = [] `SCons` emptySignal

instance EmptySig '[] where
  emptySignal = SNil

headS:: SignalL (a ': lst) -> [Signal a]
headS (SCons x _) = x
tailS :: SignalL (a ': lst) -> SignalL lst
tailS (SCons _ x) = x
data family EntityL (l::[*])

data instance EntityL '[] = ENil
data instance EntityL (x ': xs) = [x] `ECons` EntityL xs

infixr 8 `ECons`

instance Show (EntityL '[]) where
  show ENil = "ENil"

instance (Show a,Show (EntityL b)) => Show (EntityL (a ': b)) where
  show (ECons x l) = "(ECons "++show x++" "++show l++")"

headE:: EntityL ( a  ': lst) -> [a]
headE (ECons x _) = x
tailE :: EntityL ( a  ': lst) -> EntityL lst
tailE (ECons _ x) = x

class EmptyEnt (a::[*]) where
  emptyEntity :: EntityL a

instance (EmptyEnt c) => EmptyEnt (a ': c) where
  emptyEntity = [] `ECons` emptyEntity

instance EmptyEnt '[] where
  emptyEntity = ENil

class EntityW (ine :: [*]) (ins:: [*]) oute (outs :: [*]) | oute -> ine ins outs where
  wire :: Wire (Timed NominalDiffTime ()) () IO (EntityL ine,SignalL ins) ([oute],SignalL outs) -- (feedback, entities in, signal in) (entities out, signal out)

data EType =
      Sig Type |
      Delayed EType |
      Ent Type deriving (Eq,Ord)
isSig (Sig x) = True
isSig _ = False

simplifyTypes s = subRegex (makeRegex "\\s* ") (subRegex (makeRegex "(\\w+\\.)*(\\w+)") s "\\2") " "

instance Show EType where
  show (Sig t) = simplifyTypes $ pprint t
  show (Ent t) = simplifyTypes $ pprint t
  show (Delayed x) = show x

type WireType = ([EType],[EType],EType,[EType])
data WireExpr =
  WAnd {_tin::S.Set EType, _tout::S.Set EType, _expressions:: [WireExpr]} |
  WRec {_tin::S.Set EType, _tout::S.Set EType,_recVals:: S.Set EType,  _wireExpression::WireExpr} |
  WR {_tin::S.Set EType, _tout::S.Set EType, _wireType::WireType} |
  WApp {_tin::S.Set EType,_tout::S.Set EType, _leftApp::WireExpr, _rightApp::WireExpr} |
  WPass {_tin::S.Set EType, _tout::S.Set EType,_passVals::S.Set EType, _wireExpression::WireExpr} |
  WDelay {_tin::S.Set EType, _tout::S.Set EType}
  deriving (Eq,Ord)
makeLenses ''WireExpr

instance Show WireExpr where
  show (WAnd _ _ exprs) = "(WAnd " ++ show exprs ++" )"
  show (WRec _ _ vals expr) = "(WRec " ++(show $ S.toList $ vals )++ " "++ show expr ++" )"
  show (WR _ _ (_,_,t,_)) =  simplifyTypes $ "(WR " ++ show t ++" )"
  show (WApp _ _ l r) = "( " ++ show l ++ " >>> " ++ show r++" )"
  show (WPass typeIn tout pass expr) = "(WPass " ++ (show $ S.toList $ pass )  ++" )"
  show (WDelay _ _) = "WDelay"

fromJust' (Just x) = x
fromJust' Nothing = error "fromJust' there was nothing"

type EntityId = (TypeRep,Int)

class HasId a where
  ident::Traversal' a EntityId
  ident = ignored
  _ident:: a -> (Maybe EntityId)
  _ident = (^?ident)

wireExprToDot expression = do
  let nextName = state $ \(m,i) -> (show i,(m,i+1))
      toGraph :: String -> WireExpr -> StateT (M.Map String (S.Set String,String),Int) IO [String]
      toGraph node (WAnd _ _ exprs) = do
        names <- mapM (const nextName) exprs
        _1 %= M.insert node (S.fromList names, node++" [label=\""++"WAnd"++"\" shape=triangle];")
        concat <$> mapM (uncurry toGraph) (zip names exprs)
      toGraph node (WRec _ _ vals expr) = do
        exprName <- nextName
        endRecName <- nextName
        _1 %= M.insert node ([exprName],node++" [label=\""++"WRec "++(simplifyTypes.show $ S.toList $ vals )++"\" shape=diamond];")
        endPoints <- toGraph exprName expr
        mapM (\n -> do
            _1.at n %= (Just . maybe (S.fromList [endRecName], "") (first (S.insert endRecName)))
          ) endPoints
        _1 %= M.insert endRecName ([node],endRecName++" [label=\""++"WRecEnd"++"\" shape=diamond];")
        return [endRecName]
      toGraph node (WR _ _ (_,_,expr,_)) = do
        _1 %= M.insert node (S.empty,node++" [label=\"wire "++simplifyTypes (show expr)++"\" shape=box];")
        return [node]
      toGraph node (WApp _ _ l r) = do
        rname <- nextName
        lends <- toGraph node l
        mapM (\n -> do
            _1.at n %= (Just . maybe (S.fromList [rname], "") ( first (S.insert rname)))
          ) lends
        toGraph rname r
      toGraph node (WPass tin _ pass expr) = do
        ename <- nextName
        _1 %= M.insert node ([ename],node++" [label=\"pass "++simplifyTypes (show (S.toList pass))++"\" shape=diamond];")
        ends <- toGraph ename expr
        return (node:ends)
      toGraph node (WDelay _ _) = do
        _1 %= M.insert node (S.empty,node++" [label=\"Delay\" shape=box];")
        return [node]
  (m,_) <- execStateT (toGraph "1" expression) (M.empty,2)
  let declarations = foldl1 (++) (map ((++"\n").snd.snd) $ M.toList m)
      edges = foldl1 (++) (concatMap (\(node,(s,_)) -> map (\x->node++"->"++x++";") (S.toList s)) (M.toList m))
  return $ "digraph chart{\ngraph[splines=ortho];" ++ declarations ++ "\n" ++ edges ++ "\n}"



generateNetwork :: Q Exp
generateNetwork = do
  (ClassI dec instances) <- reify $ mkName "EntityW"
  let toList (SigT (AppT (AppT _ x ) xs) _) = x : toList xs
      toList (SigT PromotedNilT _ ) = []
      --should help match type aliases
      toNormalForm (AppT  x  xs)   = do
         t <- toNormalForm x
         ts <- toNormalForm xs
         return $ AppT t ts
      toNormalForm (ConT name) =do
        val <- reify name
        case val of
          (TyConI (TySynD _ _ t)) -> toNormalForm t
          _ -> return (ConT name)
      toNormalForm (SigT t s) = (`SigT` s) <$> toNormalForm t
      toNormalForm ListT = return ListT
      toNormalForm (TupleT i) = return (TupleT i)
      toNormalForm (VarT i) = fail "Type variables produce unsolvable ambiguity in the graph and are thus not allowed"
      toNormalForm (LitT t) = return (LitT t)
      toNormalForm ArrowT = return ArrowT
      checkDelay x@(Ent (AppT (ConT name ) t)) = if name == ''Delayed then  Delayed (Ent t) else x
      checkDelay x = x
  types <- mapM (\(InstanceD _ _ (AppT (AppT (AppT (AppT _ ein) sigin) eout) sout) _)-> do
      ein' <- mapM (fmap (checkDelay.Ent) . toNormalForm) $ toList ein
      sigin' <- mapM (fmap Sig . toNormalForm) $ toList sigin
      eout' <- Ent <$> toNormalForm eout
      sout' <- mapM (fmap Sig . toNormalForm) $ toList sout
      return (ein', sigin', eout', sout')
    ) instances
  let toWireExpression w@(wein,wsin,weout,wsout) = --adds self recursion
          let isDelay (Delayed _ ) = True
              isDelay _ = False
              unDelay (Delayed x) = x
              unDelay x = x
              delayed = filter isDelay wein
              wr@(WR tin tout _) = WR (S.fromList $ map unDelay wein ++ wsin) (S.fromList $ weout:wsout) w
              wr' = WApp tin tout wr (WPass tout tout (tout `S.difference` S.fromList wsout) (WDelay (S.fromList wsout) (S.fromList wsout)))
              delayedSet = S.fromList $ map unDelay delayed
              wr'' = if null delayed then wr' else WApp tin tout (WPass tin tin (tin `S.difference` delayedSet) (WDelay delayedSet delayedSet)) wr'
          in do
           --runIO (print $ "d " ++ show delayed ++ " "  ++ show wein)
           return $ wr -- if S.null (tin `S.intersection` tout) then wr'' else WRec tin tout (tin `S.intersection` tout) wr''
  expressions <- mapM toWireExpression types
  let goesTo = foldl' (\m a->
         S.foldl' (\m v -> m & at v %~ (Just . S.insert a. fromMaybe S.empty)) m (a^.tin)
        ) M.empty expressions :: M.Map EType (S.Set WireExpr)
      leavesFrom = foldl' (\m a->
          S.foldl' (\m v -> m & at v %~ (Just . S.insert a. fromMaybe S.empty)) m (a^.tout)
        ) M.empty expressions :: M.Map EType (S.Set WireExpr)
      isEnt (Ent _) = True
      isEnt _ = False
      getEin x =
        let f (WR _ _ (ein,_,_,_)) = Just ein
            f (WDelay _ _) = Nothing
            f (WPass _ _ _ x) = f x
            f (WRec _ _ _ x) = f x
            f (WApp _ _ l r) = f l <|> f r
        in fromJust (f x)
      isRoot e = not $ any isEnt (getEin e)
      (root:lst,rest) =  if any isRoot expressions then partition isRoot expressions else error "no possible root wire"
  let open = S.fromList (lst++rest)
      tree = reverse $ fix (\f l outputs open ->
          if S.null open then
            l
          else
            let (nextLevel,open') = S.partition (\e ->
                    S.null ( (S.fromList $ filter isEnt (getEin e)) `S.difference` outputs)
                  ) open
                outputs' = outputs `S.union` (S.foldl (S.union) S.empty $ S.map (S.filter isEnt.(^.tout)) nextLevel)
            in if S.null nextLevel then error ("illegal cycle detected "++show open'++" "++show outputs) else f (nextLevel:l) outputs' open'
        ) [] (S.filter isEnt (root ^.tout)) open
      inputs = fix (\f l ->
        let getIn x = S.foldl S.union S.empty (S.map (^.tin) x)
            getOut x =  S.foldl S.union S.empty (S.map (^.tout) x)
        in case l of
              [x] -> [(getIn x,getOut x)]
              x:xs ->
                let l@((tin,tout):_) =  f xs
                in (getIn x `S.union` tin,getOut x `S.union` tout):l
            ) tree
      wireExpression = fix (\ f e t ->
          let pass e s = if S.null s then e else
                WPass ((e^.tin) `S.union` s) ((e^.tout) `S.union` s) s e
          in case t of
              [(x,(typesIn,typesOut))] ->
                let xs = S.toList x
                    x' = WAnd  (foldl1' S.union $ map (^.tin) xs)
                                (foldl1' S.union $ map (^.tout) xs)
                                xs
                    passValsF = (e^.tin) `S.intersection` (e^.tout)
                    passValsB = (x'^.tin) `S.difference` (e^.tout)
                    e' = pass e passValsB
                    x'' = pass x' passValsF
                    expr = WApp (e'^.tin) (x''^.tout) e' x''
                    recVals = (expr^.tin) `S.intersection` (expr^.tout)
                in if S.null recVals then
                      expr
                    else
                      WRec ((expr^.tin) `S.difference` (expr^.tout)) (expr^.tout) recVals expr
              (x,(i,o)):rest@((_,(i',o')):_ )->
                let xs = S.toList x
                    x' = WAnd  (foldl1' S.union $ map (^.tin) xs)
                                (foldl1' S.union $ map (^.tout) xs)
                                xs
                    passValsF = ((e^.tin) `S.intersection` (e^.tout)) `S.union` (i' `S.intersection` (e^.tout))
                    passValsB = (i `S.difference` (e^.tout)) `S.union` (S.filter isSig (x'^.tin) `S.intersection` o)
                    e' = pass e passValsB
                    x'' = pass x' passValsF
                    expr = WApp (e'^.tin) (x''^.tout) e' x''
                    recVals = ((expr^.tin) `S.intersection` (expr^.tout)) `S.difference` i'
                    efinal =
                       if S.null recVals then
                          expr
                        else
                           WRec (expr^.tin) ((expr^.tout) `S.difference` recVals) recVals expr
                in f efinal rest
        ) root (zip tree inputs)

  let intersperseE :: Exp -> [Exp] -> Exp
      intersperseE name = foldl1' (\e expr -> UInfixE e name expr)
      intersperseP :: Name -> [Pat] -> Pat
      intersperseP name = foldl1' (\e expr -> UInfixP e name expr)
      nameTable names str = mapM (\t-> (t,) <$>  newName str) names
      convertToExpression (WAnd typesIn typesOut expressions) = do
        exprs <- mapM convertToExpression expressions
        --runIO $ print "WAnd"
        let types = S.toList typesIn
            (ex:exs) = reverse expressions
        unflattenArrow <- do
          names <- mapM (\t-> (t,) <$>  newName "valIn") types
          return $ VarE 'arr `AppE` ParensE
            (LamE [TupP (map (VarP . snd) names)]
              (foldl'
                (\e expr-> TupE [TupE $ map (VarE . fromJust.(`lookup` names)) (S.toList $ expr^.tin),e])
                (TupE $ map (VarE . fromJust.(`lookup` names)) (S.toList $ ex^.tin))
                exs
              )
            )
        flattenArrow <- do
          names@(nx:nxs) <- mapM (\e -> mapM (\t->(t,) <$> newName "out") (S.toList $ e^.tout)) (reverse expressions)
          let concatNames = concat names
          return $ VarE 'arr `AppE` ParensE (LamE [foldl' (\e nameList -> TupP [TupP (map (VarP . snd) nameList),e]) (TupP (map (VarP . snd) nx)) nxs]
            (TupE (map (\t-> let names = map snd $ filter ((==t).fst) concatNames
                             in intersperseE (VarE '(++)) (map VarE names) )
                        (S.toList typesOut)
              )))
        -- runIO $ do
        --   print $ pprint unflattenArrow
        --   print $ pprint flattenArrow
        return $
          UInfixE (ParensE unflattenArrow)  (VarE '(>>>)) $ UInfixE (ParensE (intersperseE (VarE '(***)) (map ParensE exprs)))  (VarE '(>>>)) (ParensE flattenArrow)
      convertToExpression (WRec typesIn typesOut recVals expr) = do
        expr' <- convertToExpression expr
        --runIO $ print "WRec"
        let recTypes = S.toList recVals
        larrow <- do
          lnames <- mapM (\t -> (t,) <$> newName "valIn") (S.toList typesIn)
          recNames <- mapM (\t -> (t,) <$> newName "rec") recTypes
          let names = lnames ++ recNames
          return $ VarE 'arr `AppE` ParensE (
              LamE [TupP [TupP $ map (VarP . snd) lnames, TupP $ map (VarP . snd) recNames]]
                   (TupE $ map (\t-> intersperseE (VarE '(++)) $ map (VarE . snd) $ filter ((==t).fst) names) (S.toList (expr^.tin)))
            )
        rarrow <- do
          names <- mapM (\t -> (t,) <$> newName "valIn") (S.toList (expr^.tout))
          return $ VarE 'arr `AppE` ParensE (
              LamE [TupP [TupP $ map (VarP . snd) names]]
                   (TupE [TupE $ map (VarE .fromJust.(`lookup` names)) (S.toList typesOut),TupE $ map (VarE .fromJust'.(`lookup` names)) recTypes])
            )
        return $
          VarE 'loop `AppE` ParensE (UInfixE (AppE (VarE 'second) $ ParensE (AppE (VarE 'delay) (TupE $ map (const $ ListE []) recTypes))) (VarE '(>>>))
            (ParensE (UInfixE larrow (VarE '(>>>)) (UInfixE expr' (VarE '(>>>)) rarrow))))
      convertToExpression (WR tin tout (entIn,sigIn,entOut@(Ent entityTypeOut),sigOut)) = do
          --runIO $ print "WR"
          larrow <- do
            inNames <- mapM (\t -> (t,) <$> newName "valIn") (S.toList tin)
            --filterExp <- [|filter (maybe True (== typeRep (Proxy :: Proxy $(return entityTypeOut)).fst))|]
            let toEntListExpression (Delayed x) = ParensE (VarE 'map `AppE` ConE 'D `AppE` toEntListExpression x)
                toEntListExpression x = VarE (fromJust (lookup x inNames))
            return $ VarE 'arr `AppE` ParensE (
                LamE [TupP (map (VarP . snd) inNames)]
                     (TupE [intersperseE (ConE 'ECons) (map toEntListExpression entIn ++ [ConE 'ENil]),
                            intersperseE (ConE 'SCons) (map (VarE . fromJust . (`lookup` inNames)) sigIn ++ [ConE 'SNil])
                     ])
              )
          rarrow <- do
            sigNames <- mapM (\t -> (t,) <$> newName "sig") sigOut
            let names = reverse $ map (VarP . snd) sigNames
            entityName <- newName "ent"
            return $ VarE 'arr `AppE` ParensE (
                LamE [TupP [VarP entityName,foldl' (\e t-> ConP 'SCons [t,e]) (ConP 'SNil []) names]]
                     (TupE $ map (\t -> intersperseE (VarE '(++)) $ map (VarE . snd) $ filter ((==t).fst) ((entOut,entityName):sigNames))  (S.toList tout))
              )
          t <- [t|Wire (Timed NominalDiffTime ()) () IO|]
          let fromEnt (Delayed x) = ConT ''Delayed `AppT` fromEnt x
              fromEnt (Ent x) = x
              fromEnt (Sig x ) = x
              toListType (x:xs) = AppT (AppT PromotedConsT (ParensT (fromEnt x))) (ParensT (toListType xs))
              toListType [] = PromotedNilT
              inTyp  = TupleT 2 `AppT` AppT (ConT ''EntityL) (toListType entIn) `AppT` AppT (ConT ''SignalL)  (toListType sigIn)
              outTyp = TupleT 2 `AppT` AppT ListT (fromEnt entOut) `AppT` AppT (ConT ''SignalL)  (toListType sigOut)
              typ = t `AppT` ParensT inTyp `AppT` ParensT outTyp
          --runIO $ print (toListType sigOut)
          return $ UInfixE larrow  (VarE '(>>>)) $ UInfixE (ParensE (SigE (VarE 'wire) typ))  (VarE '(>>>)) rarrow
      convertToExpression (WApp typesIn typesOut l r) = do
        unless ((r^.tin) `S.isSubsetOf` (l^.tout)) (fail "invalid append")
        unless ((l^.tin)==typesIn && (r^.tout)==typesOut) (fail "invalid append")
        l' <- convertToExpression l
        r' <- convertToExpression r
        glueArrow <- do
          names <- nameTable (S.toList (l^.tout)) "tin"
          return $ AppE (VarE 'arr) (LamE [TupP (map (VarP . snd) names)]
                                          (TupE (map (VarE . fromJust .(`lookup` names)) (S.toList (r^.tin)) )))
        --runIO $ print "WApp"
        return (UInfixE (ParensE l') (VarE '(>>>)) (UInfixE glueArrow (VarE '(>>>)) (ParensE r')))
      convertToExpression (WPass typesIn typesOut passVals expr) = do
        ex <- convertToExpression expr
        --runIO $ print "WPass"
        larrow <- do
          names <- nameTable (S.toList typesIn) "valIn"
          runIO $ do
            --print names
            --print (expr^.tin)
            --print $ map (`lookup` names) (S.toList (expr^.tin))
            return ()
          return $ VarE 'arr `AppE` ParensE (
                LamE [TupP $ map (VarP . snd) names]
                     (TupE [TupE $ map (VarE . fromJust.(`lookup` names)) (S.toList passVals) ,
                            TupE $  map (VarE . fromJust.(`lookup` names)) (S.toList (expr^.tin))
                            ])
            )
        rarrow <- do
          passNames <- nameTable (S.toList passVals) "pass"
          outNames <- nameTable (S.toList (expr^.tout)) "out"
          let table = passNames ++ outNames
          return $ VarE 'arr `AppE` ParensE (
                LamE [TupP [ TupP $ map (VarP . snd) passNames,TupP $ map (VarP . snd) outNames ]]
                     (TupE $ map (\t -> intersperseE (VarE '(++)) (map (VarE . snd) $ filter ((==t).fst) table)) (S.toList typesOut))
            )
        return $ UInfixE larrow (VarE '(>>>)) (UInfixE (VarE 'second `AppE` ParensE ex) (VarE '(>>>))  rarrow)
      convertToExpression (WDelay tin _) = do
        --runIO $ print "WDelay"
        return $ ParensE (VarE 'delay `AppE` TupE (map (const$ ListE []) (S.toList tin)))
  expression <- do
    expr <- convertToExpression wireExpression
    let emptyTup = TupE (map (const $ ListE []) (S.toList $ wireExpression^.tin))
    return $ UInfixE (VarE 'mkConst `AppE` ParensE (ConE 'Right `AppE` emptyTup)) (VarE '(>>>)) expr
  x <- newName "x"
  runIO $ do
    --print open
    --print wireExpression
    let edges = concatMap (\(k,s) -> (,k,) <$> (S.toList $ fromMaybe S.empty $ leavesFrom^.at k) <*> (S.toList s)) $ M.toList goesTo
        values = S.fromList $ map (\(a,_,_) -> a) edges ++ map (\(_,_,a) -> a) edges
        indx a = show $ fromJust $ S.lookupIndex a values
        dot = "digraph  DataGraph {\n" ++
              mconcat (snd $ mapAccumL (\x v -> (x+1,show x ++ "[label=\""++(filter (/='\n') (show v))++"\"];\n")) 0 (S.toList values)) ++
              mconcat (map (\(x,y,z)-> indx x ++ " -> " ++ indx z ++"[label=\""++(filter (/='\n') (show y))++"\"];\n") edges) ++
              "}"

    writeFile "data.dot" dot
    writeFile "network.dot" =<< wireExprToDot wireExpression
    print $ "root "++show root
    mapM (print.("tree "++).show) tree
    print open
    --writeFile "expr.text" $ pprint expression
    --print.pprint $ SigT (AppT  (AppT PromotedConsT  (VarT x)) PromotedNilT) (AppT ListT StarT)
    --print $ pprint $ (UInfixE (VarE x) (VarE '(+)) (UInfixE (VarE x) (VarE '(+)) (VarE x)))
    --print $ pprint $ UInfixE (UInfixE (VarE x) (VarE '(+)) (VarE x)) (VarE '(+)) (VarE x)
    hFlush stdout-- flush buffer
    return ()
  --runIO (print $ head types)

  return expression
  --let types = map (\(InstanceD _ _ tp _) -> ) instances
