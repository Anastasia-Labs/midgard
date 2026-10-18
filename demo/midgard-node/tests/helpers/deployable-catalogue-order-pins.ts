/**
 * The order pins of `deployable-scripts.test.ts`, one set per catalogue
 * generation.
 */

// These digests pin ORDER, not script bytes: the manifest order is the key
// order of `contract-deployment-info.json` (and so its digest), and the
// publication order decides which reference scripts share a publication batch.
// Script bytes are pinned by the bundle digests in `midgard-contracts.test.ts`.
//
// Every pin below, including the pre-history and pre-pool generations, was
// re-derived from the current catalogue when the missingNativeScriptTx and
// missingNativeScriptUtxo families (15 contracts and roles) were removed;
// missingScriptSource supersedes these families. The earlier digests and
// counts included those roles, so the older generations now describe the
// catalogue as it was before each addition, less those 15 roles.
//
// They were re-derived the same way when the cross-block duplicate-event
// family (two contracts and roles) was deleted. Inserting its two adjacent
// step entries back at one position reproduced every previous digest of
// every generation, so the deletion removed exactly those two entries and
// moved nothing else.
//
// They were re-derived the same way when the post-UTxO minimum-Ada mode
// retired its step-02 UTxO yield and step-05 (two contracts and roles).
// Inserting the yield before the correction-lock spend and step-05 after
// step-04 (in the manifest, the publication order and the node-runtime
// command) reproduced every previous digest of every generation. So did the
// three retired final-4 claim yields, inserted back after the L2 replay yield.
//
// They were re-derived the same way when the missing-redeemer family grew
// from its seven step contracts and roles to the 39 of the direct-derivation
// graph. Replacing its contiguous block with the previous seven entries
// reproduced every previous digest and count of every generation.
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "6099a94ac2b353adcb63f0cccd044d1f2b4b009302acfd96b2e0b86a33546c1f";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "12e44a5ad0eaf58ab55ce020cf192c42c5d5d71b450b8a76de2cd51b9934a5ac";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "4554c110013c1b166311da8fa0faffab453b25ce98aa356d3a7e393762bc459b";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "e7d8cc436f1e2043df06917e5ac29437c1a3da58db09587c0efaabc23a9c4cab";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "119c1d504980660ccb2224ce2540d85c565b4d887e4e02c328e0ba62a73afa76";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "360a6204eb8dbe8a83ffb6b74fb17956561dfb4a32da8b8a23102dae6364e3b5";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "5592aedfedc114350f4d1172bbe11fcba2a6f04961d04f282942d8faca860cab";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "5d2702665a55a52b5245fbfe16def5700447990785af2c86e10c3eb979894c29";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "f3ff7d2dd5bd559a1456a7ee72b19ae4a5dddfafa855fa73de371e7367c07900";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "ea5d0020020e3549776c4cbdb872c8dac9d350e8cdaa1bcc4ee1418d86c2036a";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "ac70f38fcfaca6c9ade48682a3efacf016437a4009e9d0a33c32bf371dcf0026";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "45217cda5e86a5b793bf04b772425b5c6cbbd94b48c47d4aeef8fbcd06c6810c";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "96492d1db5fd1304541fececbeed80cae25e9db36ec131030bca072075afd865";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "b2700bf49c448290b4570c7f67ab7e15022c400d6c5a3253ecb323a6cec646c9";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "b295e2ae6934951146c464130c01da946f65760e08b4c779e3dd72f9b35057e3";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "328f2db7911b66d8272d0bf08f1eca817508e805cd8746d21280c56526dbd51a";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "7675980d3ea2295bdc12aae8344cf447da1a50b036ef7e7c078a408d52d0de8e";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "fa5a69cd82343433258fac24b73fa602bdbabfd8a7426c8ebd5dbf170cab3b7e";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "cf29022e2e70dee535d00c2949940906ea79e19a47b449c41e19c8d5b15e51eb";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "fbd46dd4931d9f46110b049c11fda3015ca82eb94569da2b7255510f53971b14";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "d509e5c3a560aeb3a17313a0d916cef1aaea404a6d5ade3db7e9bab32333adea";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "802a42fd5030314361a34cf266c44ec9d2ac8442042bbf69ae3ce744aca7d8d2";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "e412a78bd99365b2aa4a37ade1cad6fcd729b201b9a5feb3198a0d4a3800ed9d";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "5a1b9d3f36edb0b340da7211554d55a85ea3e281188fbf6a21f5adbd979292c1";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "6418b79b8cc28b48e6e854936bdaddcc50a45b7a1b1a52e383b9e222db17df29";
export const REAL_COMMAND_ORDER_DIGEST =
  "0e1e012832613d841dd42f21985af6fee3b03bc3938c96285c657f258862343e";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "35630c96b1afd966e95435c65c8218e13cdda46b50d4998621bc725c7487c48e";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "3db815ca560383741b38126a94d292f20635b54afe745fc69ecba3940618d5bd";
