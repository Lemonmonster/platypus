{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedLists        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE LambdaCase #-}
module Entity (
  EntityW,
  wire,
  HasId,
  ident,
  _ident,
  Delayed (..),
  fromDelay,
  EntityId,
  generateNetwork,
  module HList
)where

import           Control.Lens
import           Control.Monad.State
import           Control.Wire        hiding (at, unless, when)
import           Data.Function       (fix)
import           Data.List
import qualified Data.Map.Strict     as M
import           Data.Maybe
import qualified Data.Set            as S
import           Data.Typeable
import           Debug.Trace
import           HList
import           Language.Haskell.TH
import           Prelude             hiding (id, (.))
import           System.IO
import           Text.Regex
import           Text.Regex.PCRE
import Control.DeepSeq hiding (force)

class EntityW (ine :: [*]) (ins:: [*]) oute (outs :: [*]) | oute -> ine ins outs where
  wire :: Wire (Timed NominalDiffTime ()) () IO (EntityL ine,SignalL ins) ([oute],SignalL outs) -- (feedback, entities in, signal in) (entities out, signal out)

data Delayed a  = D{_delayedPayload::a}
makeLenses ''Delayed

instance (Show a) => Show (Delayed a) where
  show (D x) = "(D "++show x++")"



fromDelay :: Delayed a -> a
fromDelay (D x) = x

data EType =
      Sig Type |
      Delayed EType |
      Ent Type deriving (Eq,Ord)
isSig (Sig x) = True
isSig _       = False
isDelay (Delayed _ ) = True
isDelay _            = False
unDelay (Delayed x) = x
unDelay x           = x

simplifyTypes s = subRegex (makeRegex "\\s* ") (subRegex (makeRegex "(\\w+\\.)*(\\w+)") s "\\2") " "

instance Show EType where
  show (Sig t)     = simplifyTypes ( pprint t )
  show (Ent t)     = simplifyTypes ( pprint t )
  show (Delayed x) = "Delayed " ++ show x

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
  show (WR _ _ (_,_,t,_)) = "(WR " ++ show t ++" )"
  show (WApp _ _ l r) = "( " ++ show l ++ " >>> " ++ show r++" )"
  show (WPass typeIn tout pass expr) = "(WPass " ++ (show $ S.toList $ pass )  ++" )"
  show (WDelay _ _) = "WDelay"

fromJust' (Just x) = x
fromJust' Nothing  = error "fromJust' there was nothing"


class HasId a where
  ident::Traversal' a EntityId
  ident = ignored
  _ident:: a -> (Maybe EntityId)
  _ident = (^?ident)

instance (HasId a) => HasId (Delayed a) where
  ident = delayedPayload.ident

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
      toList (SigT PromotedNilT _ )         = []
      --should help match type aliases
      toNormalForm (AppT  x  xs)   = do
         t <- toNormalForm x
         ts <- toNormalForm xs
         return $ AppT t ts
      toNormalForm (ConT name) =do
        val <- reify name
        case val of
          (TyConI (TySynD _ _ t)) -> toNormalForm t
          _                       -> return (ConT name)
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
          let delayed = filter isDelay wein
              wr@(WR tin tout _) = WR (S.fromList $  wein ++ map Delayed wsin) (S.fromList $ weout:wsout) w
          in do
           return wr
  expressions <- mapM toWireExpression types
  let goesTo = foldl' (\m a->
         S.foldl' (\m v -> m & at v %~ (Just . S.insert a. fromMaybe S.empty)) m (a^.tin)
        ) M.empty expressions :: M.Map EType (S.Set WireExpr)
      leavesFrom = foldl' (\m a->
          S.foldl' (\m v -> m & at v %~ (Just . S.insert a. fromMaybe S.empty)) m (a^.tout)
        ) M.empty expressions :: M.Map EType (S.Set WireExpr)
      isEnt (Ent _) = True
      isEnt _       = False
      getEin x =
        let f (WR _ _ (ein,_,_,_)) = Just ein
            f (WDelay _ _)         = Nothing
            f (WPass _ _ _ x)      = f x
            f (WRec _ _ _ x)       = f x
            f (WApp _ _ l r)       = f l <|> f r
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
      -- (all the inputs of this and later levels, all the outputs of this and previous levels )
      inputs = fix (\f l ->
        let getIn x = S.foldl S.union S.empty (S.map (^.tin) x)
            getOut x =  S.foldl S.union S.empty (S.map (^.tout) x)
        in case l of
              [x] -> [(getIn x, getOut x)]
              x:xs ->
                let ls@((tin,tout):_) = f xs
                in  (getIn x `S.union` tin,getOut x `S.union` tout):ls
            ) tree 
      wireExpression = fix (\ f e t ->
          let pass e s = if S.null s then e else
                WPass ((e^.tin) `S.union` s) ((e^.tout) `S.union` s) s e
              --special intersection operation that includes signals in the second set if
              --delayed signals exist in the first set
              --this is not a true intersection
              intersection' s1 s2 =
                S.filter (\x ->
                       Delayed x `S.member` s1 || x `S.member` s1
                   ) s2
              intersection2' s1 s2 =
                S.filter (\case
                      Delayed x ->   x `S.member` s2
                      x -> x `S.member` s2
                   ) s1
              difference' s1 s2 =
                S.filter (\case
                      Delayed x -> not $ x `S.member` s2
                      x -> not $ x `S.member` s2
                   ) s1 
          in case t of
              [(x,_)] ->
                let xs = S.toList x
                    x' = WAnd  (foldl1' S.union $ map (^.tin) xs)
                                (foldl1' S.union $ map (^.tout) xs)
                                xs
                    passValsF = intersection' (e^.tin) (e^.tout) `S.union` S.intersection (x'^.tout) (e^.tout)
                    passValsB = (x'^.tin) `S.difference` (e^.tout)
                    e' = pass e passValsB
                    x'' = pass x' passValsF
                    expr = WApp (e'^.tin) (x''^.tout) e' x''
                    recVals = intersection2' (expr^.tin)  (expr^.tout)
                in if S.null recVals then
                      expr
                    else
                      WRec (difference' (expr^.tin) (expr^.tout)) (expr^.tout) recVals expr
              (x,(i,_)):rest@((_,(i',o')):_)-> 
                let xs = S.toList x
                    x' = WAnd  (foldl1' S.union $ map (^.tin) xs)
                                (foldl1' S.union $ map (^.tout) xs)
                                xs
                    passValsF =  (intersection' (e^.tin) (e^.tout)) `S.union` (intersection' i (e^.tout) ) `S.union` (S.intersection o' (e^.tout))
                    passValsB = ((x'^.tin) `S.difference` (e^.tout)) --`S.union` (S.filter (liftA2 (||) isDelayedSig isSig) (x'^.tin) `S.intersection` o)
                    e' = pass e passValsB
                    x'' = pass x' passValsF
                    expr = WApp (e'^.tin) (x''^.tout) e' x''
                    recVals = difference' (S.difference (intersection2' (expr^.tin) (expr^.tout)) i') o' 
                    efinal =
                       if S.null recVals then
                           expr
                        else
                           WRec (S.difference (expr^.tin) recVals) (S.difference (expr^.tout) recVals) recVals expr
                in  {- trace ("xin " ++ show (x'^.tin)) $ -} f efinal rest 
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
            (LamE [TildeP $ TupP (map (VarP . snd) names)]
              (foldl'
                (\e expr-> TupE [TupE $ map (VarE . fromJust.(`lookup` names)) (S.toList $ expr^.tin),e])
                (TupE $ map (VarE . fromJust.(`lookup` names)) (S.toList $ ex^.tin))
                exs
              )
            )
        flattenArrow <- do
          names@(nx:nxs) <- mapM (\e -> mapM (\t->(t,) <$> newName "out") (S.toList $ e^.tout)) (reverse expressions)
          let concatNames = concat names
          return $ VarE 'arr `AppE` ParensE (LamE [foldl' (\e nameList -> TildeP $ TupP [TildeP $ TupP (map (VarP . snd) nameList),e]) (TildeP $ TupP (map (VarP . snd) nx)) nxs]
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
          -- runIO $ do
          --   putStrLn "wrec report start"
          --   print typesIn
          --   print typesOut
          --   print (expr^.tin)
          --   print (expr^.tout)
          --   print recNames
          --   putStrLn "wrec report end"
          return $ VarE 'arr `AppE` ParensE (
              LamE [TildeP $ TupP [TildeP $ TupP $ map (VarP . snd) lnames, TildeP $ TupP $ map (VarP . snd) recNames]]
                   (TupE $ map (\t-> VarE $ fromJust $ lookup t recNames <|> lookup t lnames ) (S.toList (expr^.tin)))
            )
        rarrow <- do
          names <- mapM (\t -> (t,) <$> newName "valIn") (S.toList (expr^.tout))
          return $ VarE 'arr `AppE` ParensE (
              LamE [TildeP $ TupP [TildeP $ TupP $ map (VarP . snd) names]]
                   (TupE [TupE $ map (VarE .fromJust.(`lookup` names)) (S.toList typesOut),
                          TupE $ map (VarE .fromJust'.(`lookup` names).(\case  Delayed a-> a; x->x;)) recTypes
                         ])
            )
        return $
          VarE 'loop `AppE` ParensE (ParensE (UInfixE larrow (VarE '(>>>)) (UInfixE expr' (VarE '(>>>)) rarrow)))
      convertToExpression (WR tin tout (entIn,sigIn,entOut@(Ent entityTypeOut),sigOut)) = do
          --runIO $ print "WR"
          let delayedSet = S.fromList $  map unDelay (filter isDelay entIn) ++ map unDelay sigIn
              undelayedSet = S.difference (S.map unDelay tin) delayedSet
              tin' = S.map unDelay tin
          -- runIO $ do
          --   print "WR start"
          --   print delayedSet
          --   print undelayedSet
          --   print "WR End"
          dArrow <- do
            inNames <- mapM (\t -> (t,) <$> newName "valIn") (S.toList tin)
            inNames' <- mapM (\t -> (t,) <$> newName "valIn") (S.toList tin')
            delayedSetNames <- mapM (\t -> (t,) <$> newName "valDelayed") (S.toList delayedSet)
            undelayedSetNames <- mapM (\t -> (t,) <$> newName "valUndelayed") (S.toList undelayedSet)
            return $ UInfixE (UInfixE (VarE 'arr `AppE` ParensE (
              LamE [TildeP $ TildeP $ TupP (map (VarP . snd ) inNames)]
              (
                TupE [TupE (map (VarE . fromJust.( liftA2 (<|>) ( (`lookup` inNames).Delayed) (`lookup` inNames) )) (S.toList delayedSet)),
                      TupE (map (VarE . fromJust.(`lookup` inNames)) (S.toList undelayedSet))]
              )
              )) (VarE '(>>>)) (VarE 'first `AppE` ParensE (VarE 'delay `AppE` TupE (map (const (ListE [])) (S.toList delayedSet)))))
               (VarE '(>>>)) (VarE 'arr `AppE` LamE [TildeP $ TupP [TildeP $ TupP $ map (VarP . snd ) delayedSetNames,TildeP $ TupP $ map (VarP . snd ) undelayedSetNames]]
                                 (TupE $ map (VarE.(\x-> fromJust (lookup x delayedSetNames <|> lookup x undelayedSetNames))) (map fst inNames'))
               )
          larrow <- do
            inNames <- mapM (\t -> (t,) <$> newName "valIn") (S.toList tin')
            --filterExp <- [|filter (maybe True (== typeRep (Proxy :: Proxy $(return entityTypeOut)).fst))|]
            let toEntListExpression (Delayed x) = ParensE (VarE 'map `AppE` ConE 'D `AppE` toEntListExpression x)
                toEntListExpression x = VarE (fromJust (lookup x inNames))
            return $ UInfixE dArrow (VarE '(>>>)) (VarE 'arr `AppE` ParensE (
                LamE [TildeP $ TupP (map (VarP . snd) inNames)]
                     (TupE [intersperseE (ConE 'ECons) (map toEntListExpression entIn ++ [ConE 'ENil]),
                            intersperseE (ConE 'SCons) (map (VarE . fromJust . (`lookup` inNames)) sigIn ++ [ConE 'SNil])
                     ])
              ))
          rarrow <- do
            sigNames <- mapM (\t -> (t,) <$> newName "sig") sigOut
            let names = reverse $ map (VarP . snd) sigNames
            entityName <- newName "ent"
            return $ UInfixE (VarE 'arr `AppE` ParensE (
                LamE [TildeP $ TupP [VarP entityName,foldl' (\e t-> ConP 'SCons [t,e]) (ConP 'SNil []) names]]
                     (TupE $ map (\t -> intersperseE (VarE '(++)) $ map (VarE . snd) $ filter ((==t).fst) ((entOut,entityName):sigNames))  (S.toList tout))
              )) (VarE '(>>>)) (VarE 'force)
          t <- [t|Wire (Timed NominalDiffTime ()) () IO|]
          let fromEnt (Delayed x) = ConT ''Delayed `AppT` fromEnt x
              fromEnt (Ent x)     = x
              fromEnt (Sig x )    = x
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
          return $ AppE (VarE 'arr) (LamE [TildeP $ TupP (map (VarP . snd) names)]
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
                LamE [TildeP $ TupP $ map (VarP . snd) names]
                     (TupE [TupE $ map (VarE . fromJust.(`lookup` names)) (S.toList passVals) ,
                            TupE $  map (VarE . fromJust.(`lookup` names)) (S.toList (expr^.tin))
                            ])
            )
        rarrow <- do
          passNames <- nameTable (S.toList passVals) "pass"
          outNames <- nameTable (S.toList (expr^.tout)) "out"
          let table = passNames ++ outNames
          return $ VarE 'arr `AppE` ParensE (
                LamE [TildeP $ TupP [ TildeP $ TupP $ map (VarP . snd) passNames,TildeP $ TupP $ map (VarP . snd) outNames ]]
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
    -- mapM_ print expressions
    --print $ "root "++show root
    --mapM (print.("tree "++).show) tree
    --print open
    --writeFile "expr.text" $ pprint expression
    --print.pprint $ SigT (AppT  (AppT PromotedConsT  (VarT x)) PromotedNilT) (AppT ListT StarT)
    --print $ pprint $ (UInfixE (VarE x) (VarE '(+)) (UInfixE (VarE x) (VarE '(+)) (VarE x)))
    --print $ pprint $ UInfixE (UInfixE (VarE x) (VarE '(+)) (VarE x)) (VarE '(+)) (VarE x)
    print "danglers"
    print (wireExpression ^. tin) 
    print (wireExpression ^. tout) 
    hFlush stdout-- flush buffer
    return ()
  --runIO (print $ head types)

  return expression
  --let types = map (\(InstanceD _ _ tp _) -> ) instances
