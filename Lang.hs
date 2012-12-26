module Lang(Id, Term(..), Program, Statement(..), M(..), ProgramResponse, Response(..), checkProgram) where

import Maybe
import List
import Control.Monad

type Id = String

data Term = Var Id 
          | Lam Id Term Term
          | App Term Term
          deriving Show

type Program = [Statement]
type ProgramResponse = [Response]

data Response = AnswerType Term Term
              | AnswerValue Term Term
              deriving Show

data Statement = Assume Id Term
               | Prove Id Term Term
               | AskType Term
               | AskValue Term
               deriving Show

type Env = [EnvFact]
data EnvFact = EnvType Id Term
             | EnvValue Id Term Term
             deriving Show

data M a = Error String
         | OK a
         deriving Show

instance Monad M where
    fail   = Error
    return = OK
    x >>= f  = case x of
                 Error msg  -> Error msg
                 OK val -> f val

applyJust :: (a -> Maybe a) -> [a] -> Maybe [a]
applyJust f []     = Nothing
applyJust f (x:xs) = maybe (return . (x:) =<< applyJust f xs) (return . (:xs)) (f x)

freeVars :: Term -> [Id]
freeVars (Var x)       = [x]
freeVars (App a b)     = freeVars a `union` freeVars b
freeVars (Lam x typ a) = (freeVars typ `union` freeVars a) \\ [x]

freshId :: Id -> [Term] -> Id
freshId x as = head . dropWhile (`elem` vs) . map (x ++) $ suffixes
    where suffixes = [""] ++ map (("_" ++) . show) [1..]
          vs       = freeVars =<< as

substitute :: Term -> Id -> Term -> Term
substitute (Var x) y s
    | x == y = s
    | x /= y = Var x
substitute (App a b) y s = App (substitute a y s) (substitute b y s)
substitute (Lam x typ a) y s
    | x == y = Lam x typ a
    | otherwise = Lam z (sub typ) (sub a)
    where z = freshId x [s]
          sub t = substitute (substitute t x (Var z)) y s

alphaEqual :: Term -> Term -> Bool
alphaEqual (Var x)     (Var y)     = x == y
alphaEqual (App a1 b1) (App a2 b2) = alphaEqual a1 a2 && alphaEqual b1 b2
alphaEqual (Lam x s a) (Lam y t b) = alphaEqual s t && alphaEqual a' b'
  where a' = substitute a x z
        b' = substitute b y z
        z  = Var $ freshId x [a, b, s, t]
alphaEqual _ _                     = False

---- Reduction
--
--reduce1 :: Term -> Maybe Term
--reduce1 (Var x)               = Nothing
--reduce1 (Lam x typ body)      = return . Lam x typ =<< reduce1 body
--reduce1 (App (Lam x typ a) b) = return (substitute a x b)
--reduce1 (App a b)             = return . lApp =<< applyJust reduce1 [a, b]
--  where lApp [x, y] = App x y
--
--reduce :: Term -> Term
--reduce = fromJust . last . takeWhile p . iterate (>>= reduce1) . return
--  where p = maybe False (const True)
--
--betaEqual :: Term -> Term -> Bool
--betaEqual term1 term2 = alphaEqual n1 n2
--  where n1 = reduce term1
--        n2 = reduce term2

---- Reduction with definitions

reduceInEnv1 :: Env -> Term -> Maybe Term
reduceInEnv1 env (Var x)               = lookupEnvValue x env
reduceInEnv1 env (Lam x typ body)      = return . Lam x typ =<< reduceInEnv1 env body
reduceInEnv1 env (App (Lam x typ a) b) = return (substitute a x b)
reduceInEnv1 env (App a b)             = return . lApp =<< applyJust (reduceInEnv1 env)[a, b]
  where lApp [x, y] = App x y

reduceInEnv :: Env -> Term -> Term
reduceInEnv env = fromJust . last . takeWhile p . iterate (>>= reduceInEnv1 env) . return
  where p = maybe False (const True)

betaEqualInEnv :: Env -> Term -> Term -> Bool
betaEqualInEnv env term1 term2 = alphaEqual n1 n2
  where n1 = reduceInEnv env term1
        n2 = reduceInEnv env term2

----

emptyEnv :: Env
emptyEnv = []

extendEnvType :: Id -> Term -> Env -> Env
extendEnvType x typ env = EnvType x typ:env

extendEnvValue :: Id -> Term -> Term -> Env -> Env
extendEnvValue x trm typ env = EnvValue x trm typ:env

envType :: EnvFact -> [(Id, Term)]
envType (EnvType x s)    = [(x, s)]
envType (EnvValue x _ s) = [(x, s)]

envValue :: EnvFact -> [(Id, Term)]
envValue (EnvValue x v _) = [(x, v)]
envValue _                = []

lookupEnvType :: Id -> Env -> Maybe Term
lookupEnvType x env = lookup x (concatMap envType env)

lookupEnvValue :: Id -> Env -> Maybe Term
lookupEnvValue x env = lookup x (concatMap envValue env)

star :: Term
star = Var "*"

typecheck :: Env -> Term -> Term -> M Term
typecheck env inferTerm typ = do
    typ' <- typeinfer env inferTerm
    if betaEqualInEnv env typ typ'
     then return typ
     else fail ("types do not match: " ++ show typ ++ " -- " ++ show typ' ++ " in env " ++ show env)

typeinfer :: Env -> Term -> M Term
typeinfer env (Var x)   = maybe (fail $ "unbound variable " ++ x) return (lookupEnvType x env)
typeinfer env (App a b) = do
    a' <- typeinfer env a
    case reduceInEnv env a' of
        Lam aVar aType aBody -> do
            case typecheck env b aType of
                OK _      -> return (substitute aBody aVar b)
                Error msg -> fail ("in application " ++ show (App a b) ++ " -- argument has wrong type\n" ++ msg)
        _ -> fail ("in application " ++ show (App a b) ++ " -- function is not an abstraction, its type is " ++ show a' ++ "\n")
typeinfer env (Lam x s a) = do
    typeinfer env s
    b <- typeinfer (extendEnvType x s env) a
    return $ Lam x s b

checkNotInEnv :: Id -> Env -> M ()
checkNotInEnv x env = maybe (return ()) (const f) $ lookupEnvType x env
  where f = fail ("identifier " ++ x ++ " defined twice in environment")

checkProgramInEnv :: Env -> Program -> M ProgramResponse
checkProgramInEnv env [] = OK []
checkProgramInEnv env (Assume x typ:prog) = do
    checkNotInEnv x env
    typeinfer env typ
    checkProgramInEnv (extendEnvType x typ env) prog
checkProgramInEnv env (Prove x typ term:prog) = do
    checkNotInEnv x env
    typeinfer env typ
    typecheck env term typ
    checkProgramInEnv (extendEnvValue x term typ env) prog
checkProgramInEnv env (AskType term:prog) = do
    typ <- typeinfer env term
    res <- checkProgramInEnv env prog
    return (AnswerType term typ : res)
checkProgramInEnv env (AskValue term:prog) = do
    typeinfer env term
    res <- checkProgramInEnv env prog
    return (AnswerValue term (reduceInEnv env term) : res)

checkProgram :: Program -> M ProgramResponse
checkProgram = checkProgramInEnv (extendEnvType "*" star emptyEnv)

