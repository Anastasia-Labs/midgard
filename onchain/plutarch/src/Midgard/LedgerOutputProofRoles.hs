{-# LANGUAGE OverloadedStrings #-}

module Midgard.LedgerOutputProofRoles (
    pstageRole,
    pdescriptorRole,
    pdescriptorRoles,
    pstageAttestationRoles,
    pspanAttestationRole,
    pscalarIntegerAttestationRole,
    pscalarBytesAttestationRole,
    pstageRoleCount,
    pdescriptorRoleCount,
    pattestationRoleCount,
) where

import Plutarch.LedgerApi.V3 (PTokenName (..))
import Plutarch.Prelude

pstageRole, pdescriptorRole :: forall s. Term s (PInteger :--> PTokenName)
pstageRole = phoistAcyclic $ plam $ \index -> pif (index #>= 0 #&& index #< 24) (pcon $ PTokenName $ pelemAt # index # pconstant @(PBuiltinList PByteString) ["V1VtLopStructureYield", "V1VtLopValueYield", "V1VtLopDatumFoldMapYield", "V1VtLopDatumFinalizeFrameYield", "V1VtLopDatumHeadScalarYield", "V1VtLopDatumAttachIntegerYield", "V1VtLopDatumFoldListYield", "V1VtLopDatumAdvanceIntegerYield", "V1VtLopRefScriptYield", "V1VtLopScriptHashYield", "V1VtLopNativeScriptYield", "V1VtLopStructureAssetsYield", "V1VtLopStructureOptionalYield", "V1VtLopStructureFinishYield", "V1VtLopDatumHeadSequenceYield", "V1VtLopDatumHeadMapYield", "V1VtLopDatumHeadLargeCtorYield", "V1VtLopDatumAttachBytesYield", "V1VtLopDatumAdvanceBytesYield", "V1VtLopDatumFinishYield", "V1VtLopDatumLargeCtorYield", "V1VtLopDatumLargeFieldsYield", "V1VtLopDatumCloseYield", "V1VtLopSpanYield"]) perror
pdescriptorRole = phoistAcyclic $ plam $ \index -> pif (index #>= 0 #&& index #< 4) (pcon $ PTokenName $ pelemAt # index # pconstant @(PBuiltinList PByteString) ["V1VtLopDescScanFactsYield", "V1VtLopDescRefScriptYield", "V1VtLopDescDatumSummaryYield", "V1VtLopDescValueSummaryYield"]) perror

pdescriptorRoles :: forall s. Term s (PBuiltinList (PAsData PTokenName))
pdescriptorRoles = pmap # plam (\index -> pdata $ pdescriptorRole # index) # pconstant @(PBuiltinList PInteger) [0, 1, 2, 3]

pspanAttestationRole, pscalarIntegerAttestationRole, pscalarBytesAttestationRole :: forall s. Term s PTokenName
pspanAttestationRole = pcon $ PTokenName $ pconstant "V1VtLopSpanYield"
pscalarIntegerAttestationRole = pcon $ PTokenName $ pconstant "V1VtLopScalarIntegerYield"
pscalarBytesAttestationRole = pcon $ PTokenName $ pconstant "V1VtLopScalarBytesYield"

pstageAttestationRoles :: forall s. Term s (PInteger :--> PBuiltinList (PAsData PTokenName))
pstageAttestationRoles = phoistAcyclic $ plam $ \index ->
    pif (index #== 5 #|| index #== 7) (pcons # pdata pscalarIntegerAttestationRole # pnil) $
        pif (index #== 17 #|| index #== 18) (pcons # pdata pscalarBytesAttestationRole # pnil) $
            pif (index #>= 0 #&& index #< pstageRoleCount) pnil perror

pstageRoleCount, pdescriptorRoleCount, pattestationRoleCount :: forall s. Term s PInteger
pstageRoleCount = 24
pdescriptorRoleCount = 4
pattestationRoleCount = 2
