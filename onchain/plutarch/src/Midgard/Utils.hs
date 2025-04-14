{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Midgard.Utils where

import Plutarch.LedgerApi.V3
    ( PRedeemer(PRedeemer),
      PScriptContext,
      PScriptInfo(PRewardingScript, PCertifyingScript),
      PTxCert(PTxCertRegStaking) )

import Plutarch.Prelude
    ( Term,
      PListLike(pelimList),
      type (:-->),
      PBool,
      plam,
      PIsData,
      PUnit,
      (#),
      pif,
      perror,
      pfield,
      pconstant,
      unTermCont,
      pmatchC )
import Plutarch.Builtin ( PIsData(pfromDataImpl), pasList )
import Plutarch.Unsafe (punsafeCoerce)


stakingWrapper :: PIsData a => Term s ((a :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking{} -> pconstant ()
        _ -> perror

    PRewardingScript{}     -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $ pif
        (validationFunction # pfromDataImpl (punsafeCoerce redData))
        (pconstant ())
        perror

    _ -> pure perror

stakingWrapper2 :: (PIsData a, PIsData b) => Term s ((a :--> b :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper2 = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking{} -> pconstant ()
        _ -> perror

    PRewardingScript{}     -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $ pelimList
        (\(pfromDataImpl . punsafeCoerce -> a) xs -> pelimList
          (\(pfromDataImpl . punsafeCoerce -> b) _ ->
              pif (validationFunction # a # b) (pconstant ()) perror
          )
          perror
          xs
        )
        perror
        (pasList # redData)

    _ -> pure perror


stakingWrapper3 :: (PIsData a, PIsData b, PIsData c) => Term s ((a :--> b :--> c :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper3 = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking{} -> pconstant ()
        _ -> perror

    PRewardingScript{}     -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $ pelimList
        (\(pfromDataImpl . punsafeCoerce -> a) xs -> pelimList
          (\(pfromDataImpl . punsafeCoerce -> b) xs' -> pelimList
              (\(pfromDataImpl . punsafeCoerce -> c) _ ->
                pif (validationFunction # a # b # c) (pconstant ()) perror
              )
              perror
              xs'
          )
          perror
          xs
        )
        perror
        (pasList # redData)

    _ -> pure perror

stakingWrapper4 :: (PIsData a, PIsData b, PIsData c, PIsData d) =>
  Term s ((a :--> b :--> c :--> d :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper4 = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking{} -> pconstant ()
        _ -> perror

    PRewardingScript{}     -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $ pelimList
        (\(pfromDataImpl . punsafeCoerce -> a) xs -> pelimList
          (\(pfromDataImpl . punsafeCoerce -> b) xs' -> pelimList
              (\(pfromDataImpl . punsafeCoerce -> c) xs'' -> pelimList
                (\(pfromDataImpl . punsafeCoerce -> d) _ ->
                  pif (validationFunction # a # b # c # d) (pconstant ()) perror
                )
                perror
                xs''
              )
              perror
              xs'
          )
          perror
          xs
        )
        perror
        (pasList # redData)

    _ -> pure perror