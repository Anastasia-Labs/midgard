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
// They were re-derived again when script-decode replaced the CEK
// program-material publication, its traversal step and four material yields,
// and when the scripts larger than one transaction (script-decode spend and
// mint, CEK core BLS budget) left the publication orders. Removing the two
// script-decode entries and restoring those six entries (and the BLS budget's
// publication) after their earlier neighbours reproduced every previous
// digest and count of every generation. Both contract sets now publish the
// same roles, so their pins coincide.
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "3f1ba9229decddad8f1270012fd462396c5aed8f37ba62b0d91fd40bff35ed81";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "5e552d51c60c580be57adb1b874754194fb8ee72014ddb25c9378ccfdd773fe9";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "c72c9f374fe3133bbee1fcdc8a8d5f1e761973f83bee2002e51a44a2c0edcdd3";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "5e552d51c60c580be57adb1b874754194fb8ee72014ddb25c9378ccfdd773fe9";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "c72c9f374fe3133bbee1fcdc8a8d5f1e761973f83bee2002e51a44a2c0edcdd3";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "285763e6a8862000d9bfdb0bb4ea4269894a75b8c6b69523e10fa4742ec60868";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "1a505edf08b3bdf7b1df2ebdccc7e8ec35283a406251db6e6d263ca1b8b94193";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "d60f19d264e3b13d75ad92c77320ac906b19afcaa6e1fdaf842160f6f84b5add";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "1a505edf08b3bdf7b1df2ebdccc7e8ec35283a406251db6e6d263ca1b8b94193";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "d60f19d264e3b13d75ad92c77320ac906b19afcaa6e1fdaf842160f6f84b5add";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "0bd225b09a0577cd221995bfa584ec0061a3e0395379b905d041c07429f4914d";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "b4944c4c1842996e78d365e8758665aa58658bd542596106f1cb7e00d1c4ab3a";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "b898efd9f1275f005a4c100a21eef45ce6393eccafbdd97c2b6de5f8d166dfe9";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "e00f83172d5cbfaaeae12711cc81081231f9c5a53f5e4619226afdc591247df9";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "b898efd9f1275f005a4c100a21eef45ce6393eccafbdd97c2b6de5f8d166dfe9";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "e00f83172d5cbfaaeae12711cc81081231f9c5a53f5e4619226afdc591247df9";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "9b0cbff821bc00bf28721cdf766e9bfa541a1c15e9d6186093b3d3f8b63874e5";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "88a588b00b6db220a7a61dd38609e0232b7d839ba96a353a9ed3563d1dee4298";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "a781d2a8d4eae04b93d1bb8b62352940c1ce270c1dee3e6fe0cf7c971ea05156";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "ded36ceafc6c399f73ae8250c76428995be6815fb9238939c90b8889c5122aa4";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "a781d2a8d4eae04b93d1bb8b62352940c1ce270c1dee3e6fe0cf7c971ea05156";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "ded36ceafc6c399f73ae8250c76428995be6815fb9238939c90b8889c5122aa4";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "445c385060650a3b5f4e284a3416d8f03aa440b41e1fdc6da99b9c2d731bb7b1";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "80445de3e731c06843653f79eefd9859914444dff559ba498e7d5e55fb7075a4";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "412d00006c6d6379ea5a30187a93a9514f5955eee0d8be52874105746a4cbee1";
export const REAL_COMMAND_ORDER_DIGEST =
  "8fb76a8900819ffe952436031ebe86228fe76581bdf8ea2259746b544381bf7a";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "412d00006c6d6379ea5a30187a93a9514f5955eee0d8be52874105746a4cbee1";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "8fb76a8900819ffe952436031ebe86228fe76581bdf8ea2259746b544381bf7a";
