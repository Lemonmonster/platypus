{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-
module Physics (
{-  PhysSim(..),
  Mover(..),
  PhysObj(..),
  Collision (..),
  acceleration,
  velocity,
  Physics.position,
  torque,
  aVelocity ,
  orientation ,
  mover ,
  shape ,
  collisionFilter ,
  inertia  ,
  aInertia  ,
  elasticity ,
  friction,
  newPhysObj,
  newMover,
  angularInertiaOf-}
) where
-}

{-
import Linear (V2(..),_xy,(^*),dot,crossZ,V3(..),cross,(^/),normalize,quadrance,signorm,norm,perp)
import Prelude hiding ((.),id)
import Geometry as G hiding (orientation)
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import Control.Lens hiding (Empty)
import Control.Monad.Writer.Lazy (tell)
import Control.Applicative
import Data.Dynamic
import Data.Maybe (catMaybes,mapMaybe,fromJust,isNothing,isJust,fromMaybe)
import Debug.Trace
import Data.Function hiding ((.),id)
import Wires hiding (at)
import Entity
import Data.Typeable
import Shape (newShape)
import Data.Ord (comparing)
import Renderer (Shaped,shape,DrawableObject(DO),color,zIndex)
import Control.Wire.Unsafe.Event -- yes we cheat a little here
import Graphics (red)

data Mover = Mover {
  _acceleration :: V2 Float,
  _velocity ::  V2 Float,
  _position :: V2 Float,
  _torque :: Float,
  _aVelocity :: Float,
  _orientation ::  Float,
  _drag :: Float,
  _mident:: (TypeRep,Int)
} deriving Show
makeLenses ''Mover

transBias = 0.1
bias x b = let dif =  (abs x/x)*b  in if abs dif <= abs x then x - dif else 0 

contactIterations = 10
collisionIterations = 5

instance HasId Mover where
  ident = mident

data Collision = Collision (TypeRep,Int) (TypeRep,Int) Manifold deriving (Show,Eq,Ord)

doMove:: Float -> Mover -> Mover--second order euler
doMove delta (Mover a v p t avel o d i) =
     Mover  a
            (v + a^*delta  - v^*d)
            (p + v^*delta )
            t
            (avel + t*delta - avel*d)
            (o + delta*avel )
            d
            i

doMoveNoVel :: Float -> Mover -> Mover--second order euler
doMoveNoVel  delta (Mover a v p t avel o d i) =
     Mover  a
            v
            (p + v^*delta)
            t
            avel
            (o + delta*avel )
            d
            i

newMover :: (TypeRep,Int) -> Mover
newMover = let z = V2 0 0 in Mover z z z 0 0 0 0.001

data PhysObj = PhysObj {
  _mover :: Mover,
  _pshape :: PhysShape,
  _collisionFilter:: (TypeRep,Int)->Bool,
  _impulseFilter::(TypeRep,Int)->Bool,
  _inertia :: Float,
  _aInertia :: Float,
  _elasticity :: Float,
  _friction :: V2 Float
}
makeLenses ''PhysObj
instance Shaped PhysObj where
  shape = pshape

_shape = _pshape

instance HasId PhysObj where
  ident = mover.mident

instance Show PhysObj where
  show (PhysObj m s _ _ i a e f) =
    "(PhysObj " ++ show m ++ " " ++ show s ++ " " ++ show i ++ " " ++ show a ++ " " ++ show e ++ " " ++ show f ++ ")"  

newPhysObj:: (TypeRep,Int) -> PhysShape -> PhysObj
newPhysObj i s = PhysObj (newMover i){_position = s^.G.position} s (const True) (const True) 1 1 0.5 (V2 0.8 0.5)

doMoveP:: Float -> PhysObj -> PhysObj--second order euler
doMoveP delta p =
     let m'= doMove delta (p ^. mover)
     in p{_mover=m', _pshape=((p ^. shape) & G.position .~ (m' ^. Physics.position)) & orient .~ (m' ^. orientation) }

doMoveNoVelP:: Float -> PhysObj -> PhysObj--second order euler
doMoveNoVelP delta p =
    let m'= doMoveNoVel delta (p ^. mover)
    in p{_mover=m', _pshape=((p ^. shape) & G.position .~ (m' ^. Physics.position)) & orient .~ (m' ^. orientation) }

squareInertia w h m =
  let l x y = sqrt (w^2 + h ^2)
      f x y = 3*h**2*l x y + l x y ** (3/2) + 3*x*y**2*log (l x y + x)
  in  (m/(w*h))*(f (w/2) (h/2) - f ((-w)/2) ((-h)/2))

angularInertiaOf :: PhysShape -> Float -> Float
angularInertiaOf (OBB (Transform _ (V2 w h) _)) m = squareInertia w h m
angularInertiaOf (Box _ (V2 w h)) m = squareInertia w h m
angularInertiaOf (Circle (Transform _ (V2 r _) _)) m = (m/(pi*r**2)) * (2*pi*r**4/4)





data QuadTree =
  Tree {
    nw::QuadTree,
    ne::QuadTree,
    sw::QuadTree,
    se::QuadTree,
    members::[(PhysObj,PhysShape)],
    size :: Int,
    box::PhysShape
  }
  | Empty deriving Show

within :: PhysShape -> PhysShape -> Bool
within (Box (V2 x1 y1) (V2 w1 h1)) (Box (V2 x2 y2) (V2 w2 h2)) =
  x2<x1 && x2+w2>x1+w1 && y2<y1 && y2+h2>y1+h1

newQTree = Tree Empty Empty Empty Empty [] 0
addToQTree :: QuadTree -> (PhysObj,PhysShape) -> QuadTree
addToQTree t@(Tree Empty Empty Empty Empty mems s b@(Box (V2 x y) d )) m
  | s == 3 =
    let dm@(V2 w h) = d/2
    in Tree (newQTree $ Box (V2 x (y+h)) dm)
            (newQTree $ Box (V2 (x+w) (y+h)) dm)
            (newQTree $ Box (V2 x y) dm)
            (newQTree $ Box (V2 (x+w) y) dm)
            (m:mems)
            (s+1)
            b
  | otherwise = t{members = m:mems}
addToQTree t@(Tree nw@Tree{box=nwb} ne@Tree{box=neb} sw@Tree{box=swb} se@Tree{box=seb} mem s _) m@(_,b)
  | b `within` nwb = t{nw = addToQTree nw m}
  | b `within` neb = t{ne = addToQTree ne m}
  | b `within` swb = t{sw = addToQTree sw m}
  | b `within` seb = t{se = addToQTree se m}
  | otherwise      = t{members = m:mem,size = s+1} 


buildTree :: [(PhysObj,PhysShape)] -> QuadTree
buildTree ojs =
  let (minx,miny, maxx,maxy) =
        foldl' (\(minx,miny,maxx,maxy) (_,Box (V2 x y) (V2 w h)) ->
                  (min minx x, min miny y,max maxx (x+w), max maxy (y+h)))
                    (1/0,1/0,negate 1/0, negate 1/0) ojs
  in foldl' addToQTree (newQTree $ Box (V2 minx miny ) (V2 (maxx-minx) (maxy-miny))) ojs

toEvents :: QuadTree -> [Collision]
toEvents Empty = []
toEvents t@Tree{members=mems} =
  let getEvents :: QuadTree -> (PhysObj,PhysShape) -> [Collision] ->  [Collision]
      getEvents Empty _ l = l
      getEvents (Tree nw ne sw se mems s qb) c@(o,sb) l
        | bound sb qb =
          let tp = filter (\ (po,bo) -> _collisionFilter o (po ^?! mover . ident) && bound bo sb ) mems
              pairsToEvents = mapMaybe (\(po,_) -> collision (_shape o) (_shape po) >>=
                        (\m -> Just $ Collision(_mident $ _mover o) (_mident $ _mover po) m))
          in  getEvents sw c $ getEvents se c $ getEvents ne c $ getEvents nw c ((pairsToEvents {-$ traceShowId-} tp) ++ l)
        | otherwise   =  l
      tailRecur Empty l = l
      tailRecur t@Tree{members=(x:xs)} l = tailRecur t{members=xs} $ getEvents t{members=xs} x l
      tailRecur (Tree ne nw se sw [] _ _) l =
        tailRecur ne $ tailRecur nw $ tailRecur se $ tailRecur sw l
  in tailRecur t []

minElasticImpulseRVel = 0.01

applyImpulse :: PhysObj -> V2 Float -> V2 Float -> PhysObj
applyImpulse o@PhysObj{_mover=m} imp r =
  o{ _mover = m { _velocity =_velocity m + (imp ^/ _inertia o),
                     _aVelocity = _aVelocity m + ((r `crossZ` imp) / _aInertia o)
                    }}

getVelAt :: PhysObj -> V2 Float -> V2 Float
getVelAt obj p =
  getVel (obj^.mover.velocity) (obj^.mover.Physics.position) (obj^.mover.aVelocity) p

getVel :: V2 Float -> V2 Float -> Float -> V2 Float -> V2 Float
getVel vel opos avel p = vel + ((V3 0 0 (avel) `cross` (pure 0 & _xy .~ (p-opos))) ^. _xy)

doImpulse' :: Maybe Float -> PhysObj-> PhysObj -> Manifold  -> (PhysObj,PhysObj)
doImpulse' restitution
          a@PhysObj{_mover=(Mover aa va pa ta ava oa _ _)}
          b@PhysObj{_mover=(Mover ab vb pb tb avb ob _ _)}
          (Manifold pnts n t d)
          =
  foldl' (\(a,b) p ->
    let distsqrd (V2 x y) = x^2 + y^2
        norm = normalize n
        testSign = signum d
        apvel = getVel va pa ava p
        bpvel = getVel vb pb avb  p
        rvel = bpvel-apvel
        ra =  p - pa
        rb =  p - pb
        ifa = ((ra `crossZ` n)^2) / _aInertia a
        ifb = ((rb `crossZ` n)^2) / _aInertia b
        ti = ifa + ifb + (1/_inertia a) + (1/_inertia b)
        e = 1 + fromMaybe ((_elasticity a + _elasticity b )/2) restitution
        imag = (e * dot norm rvel)/(ti*(fromIntegral $ length pnts))
        (V2 staticf dynamicf) = (_friction a  + _friction a )/2
        impulse = (norm ^* imag)
        nimp = norm ^* (rvel `dot` norm)
        tang = normalize $ rvel - nimp
        fric = (tang `dot` rvel)/(ti)
        fricMag = ( if abs fric < abs (imag*staticf) then fric else -imag*dynamicf )/(fromIntegral $ length pnts)
        fricv = tang ^* fricMag
        ms = a^.inertia + b^.inertia
        doImpulse o r s =
            let o' = if  (rvel `dot` n) < 0.0001 then
                        applyImpulse o (impulse^*s) r
                        else o
                o'' = if (tang `dot` rvel) > -0.0001 then
                        applyImpulse o' (fricv^*s) r
                        else o'
            in o''
    in  if _inertia a == (1/0) && _inertia b == (1/0) || (rvel `dot` n) >=  0 then
          (a,b)
        else
          (doImpulse a ra 1,doImpulse  b rb (-1))
      ) (a,b) pnts

doImpulse = doImpulse' Nothing

doMoveOut :: Map.Map EntityId PhysObj -> [Collision] -> Map.Map EntityId PhysObj
doMoveOut objs collisions =
  let moveOut :: Map.Map EntityId (PhysObj,[(V2 Float,Float)]) -> EntityId -> EntityId ->  Map.Map EntityId (PhysObj,[(V2 Float,Float)])
      moveOut mp a b =
        let (objA,vecsA) = mp^?!at a._Just
            (objB,vecsB) = mp^?!at b._Just
            maybeM = collision (objA^.shape) (objB^.shape)
        in maybe mp (\ m ->
            if _impulseFilter objA b && _impulseFilter objB a then
              let translateP p v =
                    (p & (mover . Physics.position) %~ (+v)) & (shape . G.position) %~ (+v)
                  getInertia obj vecs v =
                    _inertia obj -- + sum (map (\(vec,i) ->  max 0 (i * dot vec v)) vecs)
                  inertiaA = getInertia objA vecsA (transVec m)
                  inertiaB = getInertia objB vecsB (negate $ transVec m)
                  totalInertia = inertiaA + inertiaB
                  doTranslate obj inert s=
                    let dv = transVec m ^* ( s * bias (offset m) transBias)
                    in  translateP obj (dv ^* (
                                    if totalInertia == 1/0 then
                                      if inert == (1/0)
                                        then 0
                                        else 1
                                    else
                                        inert/totalInertia
                                      ))
              in (Map.insert b (doTranslate objB inertiaB (-1) ,(transVec m ^* (-1),inertiaA) : vecsB)
                    (Map.insert a (doTranslate objA inertiaA 1 ,(transVec m,inertiaB) : vecsA) mp))
            else mp
          ) maybeM

      --build graph of collisions
      graph = Map.fromList $ map (\((x,a):xs)-> (x , a : map snd xs)) $
          groupBy (((==EQ).).comparing fst) $ sortBy (comparing fst) $
          concatMap (\c@(Collision a b m)->[(a,(b,c)),(b,(a,c))]) collisions
      --propogate out from the objects with the highest inertia
      evalOrder = map (map (^?!ident)) $ groupBy (((==EQ).).comparing _inertia) $ sortBy (flip $ comparing _inertia)
                    $ map (fromJust.(`Map.lookup` objs).fst) $ Map.toList graph
      mpOut =  fix (\f (mp,open,closed,lst) ->
          let (mp',open',closed') = foldl' (\(mp,open',closed') x ->
                    let (mp',newOpens) = foldl' (\(mp,currOpens) (y,Collision a b m) ->
                            let modMap = moveOut mp a b
                            in (modMap,if S.member y closed' then currOpens else y:currOpens)
                          ) (mp,[]) (filter (not.(`S.member` closed').fst) $ graph^?!at x._Just)
                    in (mp',newOpens++open',S.insert x closed')
                ) (mp,[],closed) open
          in if null open' then
                if null lst then
                  Map.map fst mp'
                else
                  f (mp',filter (not.(`S.member` closed')) $ head lst,closed',tail lst)
             else
               f (mp',open',closed',lst)
        ) (Map.map (,[]) objs,[],S.empty,evalOrder)
    in mpOut

doMoveOutSimple :: Map.Map EntityId PhysObj -> [Collision] -> Int -> Map.Map EntityId PhysObj
doMoveOutSimple objs collisions n =
  let moveOut :: Map.Map EntityId PhysObj-> EntityId -> EntityId ->  Map.Map EntityId PhysObj
      moveOut mp a b =
        let objA = mp^?!at a._Just
            objB = mp^?!at b._Just
            maybeM = collision (objA^.shape) (objB^.shape) --have to re-solve due to other corrections
        in maybe mp (\m ->
              if _impulseFilter objA b && _impulseFilter objB a then
                let translateP p v =
                      (p & (mover . Physics.position) %~ (+v)) & (shape . G.position) %~ (+v)
                    totalInertia = _inertia objA + _inertia objB
                    doTranslate obj s  =
                      let dv = transVec m ^* ( s * bias (offset m) transBias)
                      in  translateP obj (dv ^* (
                                      if totalInertia == 1/0 then
                                        if _inertia obj == (1/0)
                                          then 0
                                          else 1
                                      else
                                            _inertia obj/totalInertia
                                        ))
                in (Map.insert b (doTranslate objB (-1))
                      (Map.insert a (doTranslate objA 1) mp))
              else mp
            ) maybeM
        in iterate (\ mp ->foldl' (\mp' (Collision a b m)-> moveOut mp' a b) mp collisions) objs !! n


data PhysSim = Sim {
  movers:: Map.Map (TypeRep,Int) Mover,
  physObjs:: Map.Map (TypeRep,Int) PhysObj,
  collisions :: Map.Map (TypeRep,Int) Collision
} deriving Show

instance HasId PhysSim

resolveContactGraph :: Float -> Map.Map EntityId PhysObj  -> (Map.Map EntityId PhysObj,[DrawableObject])
resolveContactGraph dt objs =
  let updated = Map.map (doMoveP dt) objs
      collisions = toEvents $ buildTree $ map (\p->(p,toBound $ _shape p)) $ Map.elems updated
      outContactGraph :: Map.Map EntityId (S.Set (EntityId,Collision))
      outContactGraph = foldl' (\mp c@(Collision a b m) ->
          foldl' (\mp (k,v)-> mp & at k %~ Just . maybe (S.singleton v) (S.insert v)) mp
            (case dot (
                let aVel = getVelAt (updated^?!at a._Just) (sum (points m)/(fromIntegral.length $ points m))
                    bVel = getVelAt (updated^?!at b._Just) (sum (points m)/(fromIntegral.length $ points m))
                 in  if aVel == 0 then bVel else aVel) (normal m)  of
              x | x == 0 -> []
                | x <  0 -> [(a,(b,c))]
                | x >  0 -> [(b,(a,c))] )
        ) Map.empty collisions
      inContactGraph :: Map.Map EntityId (S.Set EntityId)
      inContactGraph = foldl' (\mp (a,s) ->
          foldl' (\mp (v,_) ->  mp & at v %~ Just . maybe (S.singleton a) (S.insert a)) mp (S.toList s)
        ) Map.empty (Map.toList outContactGraph)
      --implementation of korsaraju's algorithm
      depthFirstOrder:: [EntityId]
      depthFirstOrder = snd $ fix (\f lst closed out-> -- first layer breaks graph into connected components
          if null lst then
             (closed,out)
          else case lst of
            (x:xs) ->
              let ~(closed',out') = foldl' (\(c,o) v -> f (v:xs) c o)
                      (S.insert x closed,out)
                      (maybe [] (filter (not.(`S.member` closed)).map fst.S.toList) (outContactGraph^.at x))
              in  if x `S.member` closed
                      then f xs closed out
                      else f xs closed' $ x:out'
        ) (map fst $ Map.toList outContactGraph) S.empty []
      sccs :: [S.Set EntityId]
      sccs =
        if null depthFirstOrder then
          []
        else
          fix (\f closed curr lst -> -- find the scc components
            let ~(component,closed') = fix (\f (component,closed) curr ->
                    if curr `S.member` closed
                       then (component,closed)
                       else foldl f (S.insert curr component,S.insert curr closed) (maybe [] S.toList $ inContactGraph^.at curr)
                  ) (S.empty,closed) curr
                out
                  | null lst = if curr `S.member` closed
                      then []
                      else [component]
                  | curr `S.member` closed =  f closed (head lst) (tail lst)
                  | otherwise = component : f closed' (head lst) (tail lst)
              in out
          ) S.empty (head depthFirstOrder) (tail depthFirstOrder)
      topSort :: (S.Set ([Collision],[(EntityId,Collision)]),
                    Map.Map ([Collision],[(EntityId,Collision)]) [([Collision],[(EntityId,Collision)])])
      topSort@(start,sccGraph) =
        if null sccs then
           (S.empty,Map.empty)
        else
          (\(closed,noIn,_,edges,_)->(
              S.map (fromJust.(`Map.lookup` closed)) noIn,
              foldl' (\mp (a,b) -> mp & at a %~ (Just . maybe [b] (b:)) ) Map.empty edges)) $
          fix (\f closed noIn sccComps (x:xs) out->
              if not (x `Map.member` closed) then
                let v@(internalCollisions,externalCollisions) =
                      foldl' (\ (ic,ec) a ->
                        foldl' (\ (ic,ec) (b,m) ->
                          if b `S.member` x then
                              (m:ic,ec)
                          else
                              (ic,(b,m):ec)
                        ) (ic,ec) (maybe [] S.toList $ outContactGraph^.at a)
                      ) ([],[]) (S.toList x)
                    ((closed',noIn',lst',out'),outNeighbors) = mapAccumL (\(closed,noIn,lst,out) s ->
                        let (closed',noIn',lst',out',v') = f closed noIn sccComps (s:lst) out
                        in ((closed',S.delete s noIn',lst',out'),v')
                      ) (Map.insert x v closed,noIn,xs,out) $ S.toList $ S.fromList $ map (\ (b,_) ->
                            fromJust $ find (b `S.member`) sccComps
                        ) externalCollisions
                in fromJust $ find (\(c,ni,ls,out,_)-> null ls) $
                      iterate (\(c,ni,ls,out,_)->f c ni sccComps ls out) (closed',noIn',xs,(map (v,) outNeighbors++out'),v)
              else
                  (closed,noIn,xs,out,fromJust $ Map.lookup x closed)
           ) Map.empty (S.fromList sccs) sccs sccs []
      contactUpdate :: Float -> Map.Map EntityId PhysObj -> Collision -> Map.Map EntityId PhysObj
      contactUpdate r objs (Collision a b m) =
         let (aMod,bMod) = doImpulse' (Just r) (objs^?!at a._Just) (objs^?!at b._Just) m
         in  {-trace (show (objs^?!at a._Just.mover.velocity,objs^?!at b._Just.mover.velocity,objs^?!at b._Just.mover.aVelocity)++
                    " "++show (aMod^.mover.velocity,bMod^.mover.velocity,bMod^.mover.aVelocity)) $-} objs & at a .~  Just aMod & at b .~ Just bMod
      shockUpdate ::  Float -> Map.Map EntityId PhysObj -> Collision -> Map.Map EntityId PhysObj
      shockUpdate r mp (Collision a b m@(Manifold pnts n t d)) =
        let  (aMod,bMod) = doImpulse' (Just r) (mp^?!at a._Just) (mp^?!at b._Just) m
             nor = normalize n
             testSign = signum d
             pa = (aMod^.mover.Physics.position)
             pb = (bMod^.mover.Physics.position)
             apvel = aMod^.mover.velocity
             bpvel = bMod^.mover.velocity
             rvel = bpvel-apvel
             deinf x = if x == 1/0 then 0 else x
             ti =  deinf (_inertia aMod) + deinf (_inertia bMod)
             impulse = nor ^* (negate $ dt * max ((100*norm (t^*d)) - (500*(dot nor rvel))) 0)
             (aMod',bMod') =(aMod & mover.velocity %~ (\v-> v + impulse * pure (deinf $ _inertia aMod/ti)) ,
                             bMod & mover.velocity %~ (\v-> v - impulse * pure (deinf $ _inertia bMod/ti)))
        in  mp & at a .~  Just aMod' & at b .~ Just bMod'
      resolve :: (Float -> Map.Map EntityId PhysObj -> Collision -> Map.Map EntityId PhysObj) ->
                  Bool ->  Map.Map EntityId PhysObj -> Float  -> Map.Map EntityId PhysObj
      resolve update shock objs restitution  =
                fst $ fromJust $ find (null.snd) $ iterate (\(objs,nodes) ->
                  second concat $ mapAccumL (\objs node@(internal,external) ->
                    let internalUpdate = foldl' (update restitution) objs internal
                        externalUpdate =
                          if shock then
                              foldl' (\objs (out,c@(Collision a b m))->
                                  let PhysObj{_inertia=inert,_aInertia=aInert} = objs^?!at out._Just
                                  in  update restitution (objs & at out %~ fmap (\obj->obj{_inertia=1/0,_aInertia=1/0})) c
                                        & at out %~ fmap (\obj->obj{_inertia=inert,_aInertia=aInert})
                                ) internalUpdate external
                          else foldl' (update restitution) internalUpdate $ map snd external
                    in (externalUpdate,fromMaybe [] $ sccGraph^.at node)
                  ) objs nodes
                ) (objs,S.toList start)
      ~contactsSolved = foldl' (resolve contactUpdate False) updated $
        map (max 0 . (\x-> x/contactIterations)) [1..contactIterations]
      outMap = Map.unionWith (\a@PhysObj{_mover=m} PhysObj{_mover=Mover{_velocity=v,_aVelocity=av}} ->
          a{_mover=m{_velocity=v,_aVelocity=av}}
        ) objs $ resolve shockUpdate True contactsSolved 0
      triangle (a,b) =
        let c =b+((a-b)/10)
            p = (perp (a - b))/10
        in  [(b,c+p),(b,c-p),(a,b)]
  in  (outMap,
       concatMap (map (DO.(zIndex .~ 10).(color .~ red).newShape.uncurry newLine).
            (\(a,s) -> concatMap ((\b ->triangle (outMap^?!at a._Just.mover.Physics.position,outMap^?!at b._Just.mover.Physics.position)).fst) $
          S.toList s)) $
            Map.toList outContactGraph)





instance EntityW '[] '[Mover,PhysObj,Event [PhysObj]] PhysSim '[DrawableObject] where
  wire =mkPure $ \ds (_,newMovers `SCons`newObjects `SCons` eventNewObject `SCons` SNil)  ->
    let dt = realToFrac $ dtime ds
        m' =  Map.map (doMove dt) (foldl' (\m x -> Map.insert (x^?!ident) x m ) Map.empty (map _payload newMovers) )
        o = foldl' (\o x -> Map.insert (x^?!mover.ident) x o )
              Map.empty (map _payload newObjects ++
                            concatMap ((\x-> case x of
                                                  Event a -> a
                                                  _ -> [])._payload) eventNewObject)
        doSim reps (o,e) =
          let getEvents mp = toEvents $ buildTree $ map (\p->(p,toBound $ _shape p)) $ Map.elems mp
              {-events =  fix (\f list out-> if null list then out else let h = head list in
                          f (tail list) $
                            foldl' (\outl (a,b,m)-> if isJust m then Collision (a^?!mover.ident) (b^?!mover.ident) (fromJust m) : outl else outl) out $
                              foldl' (\clist x-> if bound (x^.shape) (h^.shape) then (x,h, collision (x^.shape) (h^.shape) ): clist else clist) [] (tail list) ) (Map.elems o') []-}
              (o',events) =
                iterate (\ (o'',e') ->
                  let oMoved = Map.map (doMoveNoVelP $ dt/reps) o''
                      events' = getEvents oMoved
                      collisionsResolved =
                        Map.unionWith (\a b -> b & mover.Physics.position .~ a^.mover.Physics.position) o'' $
                          foldl' (\mp (Collision a b m) ->
                            let pa = o'' Map.! a
                                pb = o'' Map.! b
                            in
                              if _impulseFilter pa b && _impulseFilter pb a then
                                  let look o = fromJust . Map.lookup ( _mident $ _mover o)
                                      (pa', pb') = doImpulse (look pa mp) (look pb mp) m
                                  in (Map.insert b pb' (Map.insert a pa' mp))
                                else mp
                            ) oMoved events'
                  in (collisionsResolved,
                      Map.union e' $ Map.fromList $ concatMap (\c@(Collision a b m)->[(a,c),(b,c)]) events')
                    ) (o,e) !! 1
              (o'',graph) = resolveContactGraph (dt/reps) o'
              o''' = Map.map (doMoveNoVelP (dt/reps)) o''
              lst =  Map.elems o'''
          in ((o''',events),graph)
        ((objects,events),g) = iterate (doSim 1 . fst) ((o,Map.empty),[]) !! 1
    in  (Wires.Right ([Sim m' objects events],sigify g `SCons` SNil),wire)



instance EntityW '[PhysSim] '[] (Map.Map EntityId Mover) '[] where
  wire = first . arr $ pure.movers.head.headE

instance EntityW '[PhysSim] '[] (Map.Map EntityId PhysObj) '[] where
  wire = first . arr $ pure.physObjs.head.headE

instance EntityW '[PhysSim] '[] (Map.Map (Maybe EntityId) (Event Collision)) '[] where
  wire = first . arr $ pure . Map.insert Nothing NoEvent . Map.mapKeys Just . (Map.map Event).collisions.head.headE


-}
module Physics (
  module Struct,
  PhysSim,
  Collision(..),
  getCollision,
  getCollisionEvent,
  Collisions,
  CollisionEvents,
  involves
) where

import Physics.Hipmunk as Hip
import Entity
import Linear
import Structures as Struct
import Control.Wire hiding (at,when)
import Prelude hiding ((.),id)
import qualified Data.Map.Strict as M
import Data.Typeable
import Control.Wire.Unsafe.Event
import Signals
import Control.Applicative
import Control.Monad
import Geometry as G
import Control.Lens
import Data.StateVar
import Data.IORef
import Data.Traversable
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Data.Ord
import Data.List (sortBy,groupBy,partition)
import Control.DeepSeq
import Data.Maybe

data Collision = Collision{
  objA::EntityId,
  objB::EntityId,
  netImpulse::V2 Double,
  netImpulseWFric::V2 Double,
  normal::V2 Double,
  points:: [V2 Double ]
} deriving (Eq,Ord,Show)

involves :: EntityId -> Collision -> Bool
involves i (Collision{objA = a, objB = b}) =
  i == a || i == b

instance Show (IORef a) where
  show = const "IORef"

instance Show Shape where
  show = const "Shape"

instance Show Hip.Body where
  show = const "Hipmunk.Body"

instance Show Space where
  show = const "Space"

newtype IOAutomaton a b = IOAutomaton (a -> IO (b,IOAutomaton a b))
instance Show (IOAutomaton a b) where
  show = const "IOAutomaton"

data PhysSim  = Sim {
  bodyStates :: M.Map EntityId (Struct.Body,[Shape],Hip.Body),
  constraints :: M.Map EntityId (Struct.Constraint,IOAutomaton Struct.Constraint Struct.Constraint),
  collisions :: M.Map EntityId [Collision],
  space :: Space,
  idCount :: Int,
  ref :: IORef [(EntityId,Collision)]
} deriving (Typeable,Show)


physSim = typeRep (Proxy :: Proxy PhysSim)

toHipVec :: Iso' (V2 Double) Vector
toHipVec = iso (\(V2 x y) -> Vector x y) (\(Vector x y) -> V2 x y)
fromHipVec :: Iso' Vector (V2 Double)
fromHipVec = from toHipVec

instance EntityW '[Delayed PhysSim] '[Destroy, Create Struct.Constraint, Create Struct.Body] PhysSim '[] where
  wire = mkGen $ \ds (sim `ECons`_ , destroy `SCons` constraintEvts `SCons` createEvts `SCons` _) -> do
    let fromEvent (Event x) = x
    case map fromDelay sim of
      [] -> do
        initChipmunk
        print "chipmunk initialized"
        space <- newSpace
        ref <- newIORef []
        return (Right ([Sim M.empty M.empty M.empty space 0 ref],SNil),wire)
      [Sim bodies constraints _ space id ref] -> do
        let (creates,bodyUpdates,createWId)  = foldl (\(c,bu,cwid) x -> case x of
                (Create a) -> (a:c,bu,cwid)
                (Update a) -> (c,a:bu,cwid)
                (CNoId a ) -> (c,bu,a:cwid)
              ) ([],[],[]) $ map _payload createEvts
        let createVals = map fromEvent $ filter occurred creates
            convInf x = if x == 1/0 then infinity else x
            (updates,ucreates) = partition ((`M.member` bodies).(^?!ident))  bodyUpdates
            (id',localCreates) = mapAccumL (\i f -> (i+1,f $ EntityId (physSim,i))) id $ map fromEvent $ filter occurred  createWId
            createdBodies = map (\body-> (body^?!ident,
              let convertedShapes = map (\(BodyShape shp fric group layer rest mass svel)->
                      let doShape shp' = do
                            Hip.friction shp' $= fric
                            Hip.group shp' $= group
                            Hip.layers shp' $= layer
                            Hip.elasticity shp' $= rest
                            Hip.surfaceVel shp' $= svel^.toHipVec
                            return shp'
                      in (case shp of
                            l@(G.Line t) ->
                              let [a,b] = map (^.toHipVec) $ fst $ getPNs l
                              in  (momentForSegment (convInf mass) a b, \bod -> newShape bod (LineSegment a b 1) 0)
                            c@(G.Circle t) ->
                              let p = c^.G.position.toHipVec
                                  r = G.radius c
                              in  (momentForCircle (convInf mass) (0,r) p,\b -> newShape b (Hip.Circle r) p)
                            poly ->
                              let verts = map (^.toHipVec) $ reverse $ fst $ getPNs poly
                              in (momentForPoly (convInf mass) verts 0,\b -> newShape b (Hip.Polygon verts) 0)
                           ) & _2 %~ (>=> doShape)
                    ) (body^.Struct.shapes)
                  totalMass = convInf $ sum $ map (^.Struct.mass) (body^.Struct.shapes)
                  totalMoment = convInf $ if body^.canRotate then sum $ map fst convertedShapes else 1/0
              in do
                  hipBody <- Hip.newBody totalMass totalMoment
                  Hip.position hipBody $= body^.pos.toHipVec
                  Hip.velocity hipBody $= body^.vel.toHipVec
                  Hip.force hipBody $= body^.Struct.force.toHipVec
                  Hip.angle hipBody $= body^.Struct.angle
                  Hip.angVel hipBody $= body^.aVel
                  Hip.torque hipBody $= body^.Struct.torque
                  shapes <- mapM (($ hipBody).snd) convertedShapes
                  spaceAdd space hipBody
                  mapM_ (spaceAdd space) shapes
                  return (body,shapes,hipBody)
             )) $ createVals ++ localCreates ++ ucreates
        newBodies <- mapM (\(e,v) -> (e,) <$> v) $ M.toList $ M.unionWith (\a b -> do
            (body,shapes,hbody) <- a
            mapM_ (spaceRemove space) shapes
            spaceRemove space hbody
            b
          ) (M.map return bodies) (M.fromList createdBodies)
        updatedBodies <- foldM (\mp body -> do
            let (prevBody,hipShapes,hipBody) = mp^?!at (body^?!ident)._Just
            mapM_ (\(prev,curr,hip)-> do
                let check lens state =
                      when (prev^.lens /= curr ^.lens) (state hip $= curr^.lens)
                check Struct.group Hip.group
                check Struct.layers Hip.layers
                check Struct.elasticity Hip.elasticity
                check Struct.friction Hip.friction
                check (Struct.surfaceVel.toHipVec) Hip.surfaceVel
                return ()
              ) $ zip3 (prevBody^.Struct.shapes) (body^.Struct.shapes) hipShapes
            let check lens state =
                  when (prevBody^.lens /= body^.lens) (state hipBody $= body^.lens)
            check (pos.toHipVec) Hip.position
            check (vel.toHipVec) Hip.velocity
            check (Struct.force.toHipVec) Hip.force
            check Struct.angle Hip.angle
            check aVel angVel
            check Struct.torque Hip.torque
            return ( mp & at (body^?!ident)._Just .~ (body,hipShapes,hipBody) )
          ) (M.fromList newBodies) updates
        bodiesWithRemoved <- foldM (\mp e ->
            case mp^.at e of
              Just (body,shapes,hbody) -> do
                mapM_ (spaceRemove space) shapes
                spaceRemove space hbody
                return $ M.delete e mp
              _ -> return mp
          ) updatedBodies (map (\(Event x) -> x) $ filter occurred $ map ((\(Dest x) -> x)._payload) destroy)
        let idMap = M.fromList $ map (\(id,(_,_,b))-> (b,id) ) (M.toList bodiesWithRemoved)
        constraints <- foldM (\ (mp,id) x ->
            let convert const@(Constraint a b s@(Struct.Pin _ _) i) =
                  let f (Struct.Pin a b) = Hip.Pin (a^.toHipVec) (b^.toHipVec)
                  in do
                      let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                      constraint <- newConstraint ba bb (f s)
                      spaceAdd space constraint
                      let arrow  = IOAutomaton $ \case
                            c@(Struct.Constraint a b s@Struct.Pin{} i) -> do
                              redefineC constraint (f s)
                              return (c,arrow)
                            c -> do
                              spaceRemove space constraint
                              convert c
                      return (const,arrow)
                convert const@(Constraint a b s@(Struct.Slide pa pb min max) i) =
                  let f (Struct.Slide pa pb min max) = Hip.Slide (pa^.toHipVec) (pb^.toHipVec) min max
                  in do
                      let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                      constraint <- newConstraint ba bb (f s)
                      spaceAdd space constraint
                      let arrow  = IOAutomaton $ \case
                            c@(Struct.Constraint a b s@Struct.Slide{} i) -> do
                              redefineC constraint (f s)
                              return (c,arrow)
                            c -> do
                              spaceRemove space constraint
                              convert c
                      return (const,arrow)
                convert const@(Constraint a b s@(Struct.Pivot1 _) i) =
                  let f (Struct.Pivot1 pa) = Hip.Pivot1 (pa^.toHipVec)
                  in do
                      let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                      constraint <- newConstraint ba bb (f s)
                      spaceAdd space constraint
                      let arrow  = IOAutomaton $ \case
                            c@(Struct.Constraint a b s@Struct.Pivot1{} i) -> do
                              redefineC constraint (f s)
                              return (c,arrow)
                            c -> do
                              spaceRemove space constraint
                              convert c
                      return (const,arrow)
                convert const@(Constraint a b s@Struct.Pivot2{} i) =
                 let f (Struct.Pivot2 pa pb) = Hip.Pivot2 (pa^.toHipVec) (pb^.toHipVec)
                 in do
                     let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                     constraint <- newConstraint ba bb (f s)
                     spaceAdd space constraint
                     let arrow  = IOAutomaton $ \case
                           c@(Struct.Constraint a b s@Struct.Pivot2{} i) -> do
                             redefineC constraint (f s)
                             return (c,arrow)
                           c -> do
                            spaceRemove space constraint
                            convert c
                     return (const,arrow)
                convert const@(Constraint a b s@Struct.Groove{} i) =
                  let f (Struct.Groove (pa,pb) pivot) = Hip.Groove (pa^.toHipVec,pb^.toHipVec) (pivot^.toHipVec)
                  in do
                      let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                      constraint <- newConstraint ba bb (f s)
                      spaceAdd space constraint
                      let arrow  = IOAutomaton $ \case
                            c@(Struct.Constraint a b s@Struct.Groove{} i) -> do
                              redefineC constraint (f s)
                              return (c,arrow)
                            c -> do
                             spaceRemove space constraint
                             convert c
                      return (const,arrow)
                convert const@(Constraint a b s@Struct.Gear{} i) =
                   let f (Struct.Gear phase ratio) =Hip.Gear phase ratio
                   in do
                       let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                       constraint <- newConstraint ba bb (f s)
                       spaceAdd space constraint
                       let arrow  = IOAutomaton $ \case
                             c@(Struct.Constraint a b s@Struct.Gear{} i) -> do
                               redefineC constraint (f s)
                               return (c,arrow)
                             c -> do
                              spaceRemove space constraint
                              convert c
                       return (const,arrow)
                convert const@(Constraint a b s@Struct.DampedSpring{} i) =
                   let f (Struct.DampedSpring pa pb rest stiff damp) =
                            Hip.DampedSpring (pa^.toHipVec) (pb^.toHipVec) rest stiff damp
                   in do
                       let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                       constraint <- newConstraint ba bb (f s)
                       spaceAdd space constraint
                       let arrow  = IOAutomaton $ \case
                             c@(Struct.Constraint a b s@Struct.DampedSpring{} i) -> do
                               redefineC constraint (f s)
                               return (c,arrow)
                             c -> do
                              spaceRemove space constraint
                              convert c
                       return (const,arrow)
                convert const@(Constraint a b s@Struct.DampedRotarySpring{} i) =
                   let f (Struct.DampedRotarySpring ang stiff damp) =
                            Hip.DampedRotarySpring ang stiff damp
                   in do
                       let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                       constraint <- newConstraint ba bb (f s)
                       spaceAdd space constraint
                       let arrow  = IOAutomaton $ \case
                             c@(Struct.Constraint a b s@Struct.DampedRotarySpring{} i) -> do
                               redefineC constraint (f s)
                               return (c,arrow)
                             c -> do
                              spaceRemove space constraint
                              convert c
                       return (const,arrow)
                convert const@(Constraint a b s@Struct.Ratchet{} i) =
                  let f (Struct.Ratchet phase ratc) = Hip.Ratchet phase ratc
                  in do
                     let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                     constraint <- newConstraint ba bb (f s)
                     spaceAdd space constraint
                     let arrow  = IOAutomaton $ \case
                           c@(Struct.Constraint a b s@Struct.Ratchet{} i) -> do
                             redefineC constraint (f s)
                             return (c,arrow)
                           c -> do
                            spaceRemove space constraint
                            convert c
                     return (const,arrow)
                convert const@(Constraint a b s@Struct.RotaryLimit{} i) =
                  let f (Struct.RotaryLimit min max) = Hip.RotaryLimit min max
                  in do
                     let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                     constraint <- newConstraint ba bb (f s)
                     spaceAdd space constraint
                     let arrow  = IOAutomaton $ \case
                           c@(Struct.Constraint a b s@Struct.RotaryLimit{} i) -> do
                             redefineC constraint (f s)
                             return (c,arrow)
                           c -> do
                            spaceRemove space constraint
                            convert c
                     return (const,arrow)
                convert const@(Constraint a b s@Struct.SimpleMotor{} i) =
                  let f const@(Struct.SimpleMotor rate) = Hip.SimpleMotor rate
                  in do
                     let ((_,_,ba),(_,_,bb)) = (bodiesWithRemoved^?!at a._Just,bodiesWithRemoved^?!at b._Just)
                     constraint <- newConstraint ba bb (f s)
                     spaceAdd space constraint
                     let arrow  = IOAutomaton $ \case
                           c@(Struct.Constraint a b s@Struct.SimpleMotor{} i) -> do
                             redefineC constraint (f s)
                             return (c,arrow)
                           c -> do
                            spaceRemove space constraint
                            convert c
                     return (const,arrow)
            in  case x of
                 (Create (Event c)) -> do
                    (c',a) <- convert c
                    return (mp & at (c^?!ident) .~ Just (c',a),id)
                 (CNoId (Event f)) -> do
                    let eid = EntityId (physSim,id)
                    (c,a) <- convert (f eid)
                    return (mp & at eid .~ Just (c,a),id+1)
                 (Update c) ->
                    case mp ^. at (c^?!ident) of
                       (Just (c,IOAutomaton f)) -> do
                        (c',a) <- f c
                        return (mp & at (c^?!ident) .~ Just (c',a),id)
                       Nothing -> do
                        (c',a) <- convert c
                        return (mp & at (c^?!ident) .~ Just (c',a),id)
                 _  -> return (mp,id)
          ) (constraints,id') $ map _payload constraintEvts
        writeIORef ref []
        setDefaultCollisionHandler space Handler{
            beginHandler = Just $ return True,
            preSolveHandler = Just $ do
              (a,b)<- Hip.shapes
              let (ba,bb) = (idMap^?!at (Hip.body a)._Just,idMap^?!at (Hip.body b)._Just)
              p <- map (^.fromHipVec) <$> Hip.points
              n <- (^.fromHipVec) <$> Hip.normal
              postStep a $ do
                impulse <- (^.fromHipVec) <$> totalImpulse
                fImpulse <- (^.fromHipVec) <$> totalImpulseWithFriction
                let c =  Collision ba bb impulse fImpulse n p
                liftIO $ modifyIORef ref (\x -> (bb,c):(ba,c):x)
              let collide a = bodiesWithRemoved^?!at ba._Just._1.processCollision
              return (collide ba && collide bb)
              ,
            postSolveHandler = Nothing,
            separateHandler = Nothing
          }
        step space (realToFrac $ dtime ds)
        bodiesToOutput <-mapM (fmap (\(b,s,h)-> b `seq` (b,s,h)) . execStateT ( do
            h <- use _3
            let set lens state = do
                  x <- liftIO $ Data.StateVar.get $ state h
                  modify (_1.lens.~ x)
            set (pos.toHipVec) Hip.position
            set (vel.toHipVec) Hip.velocity
            set (Struct.force.toHipVec) Hip.force
            set bangle Hip.angle
            set aVel angVel
            set Struct.torque Hip.torque
          ))  bodiesWithRemoved
        collisions <- M.fromList .map (\l@((i,_):_)-> (i,map snd l) ) . groupBy (((==EQ).) . comparing fst) . sortBy (comparing fst)
                        <$> readIORef ref
        return $ bodiesToOutput `seq` (Right ([Sim bodiesToOutput M.empty collisions space id' ref],SNil),wire)

instance EntityW '[PhysSim] '[] (M.Map EntityId Struct.Body) '[] where
  wire = first $ arr $ map (M.map (^._1).bodyStates) . headH

instance EntityW '[PhysSim] '[] (M.Map EntityId Struct.Constraint) '[] where
  wire = first $ arr $ map (M.map (^._1) . constraints) . headH

instance EntityW '[PhysSim] '[] (M.Map (Maybe EntityId) [Collision]) '[] where
  wire = first $ arr $ map (M.insert Nothing [] . M.mapKeysMonotonic Just . collisions) . headH

instance EntityW '[PhysSim] '[] (M.Map (Maybe EntityId) (Event [Collision])) '[] where
  wire = first $ arr $ map (M.insert Nothing NoEvent . M.map (\x-> if null x then NoEvent else Event x) .
              M.mapKeysMonotonic Just . collisions) . headH


type Collisions = M.Map (Maybe EntityId) [Collision]
type CollisionEvents = M.Map (Maybe EntityId) (Event [Collision])
type Bodies = M.Map EntityId Struct.Body
type Constraints = M.Map EntityId Struct.Constraint

getCollisionEvent :: EntityId -> CollisionEvents -> Event [Collision]
getCollisionEvent id evts = fromJust $ M.lookup (Just id) evts <> M.lookup Nothing evts

getCollision :: EntityId -> Collisions -> [Collision]
getCollision id cols = fromJust $ M.lookup (Just id) cols <> M.lookup Nothing cols
