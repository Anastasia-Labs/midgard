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
//
// They were re-derived once more when the CEK core BLS budget step came back
// under the publication target and rejoined the publication orders. Removing
// its one entry reproduced every previous publication and command digest and
// count of every generation; the manifest pins did not change.
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "3f1ba9229decddad8f1270012fd462396c5aed8f37ba62b0d91fd40bff35ed81";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "71075612813c2c64cc9602924a5d8e35be598b7d8be7c60f690fcd1f9d062789";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "21095a96995398c93ed11ec29467af43d90e954df34184683b7a5e893aa7b289";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "71075612813c2c64cc9602924a5d8e35be598b7d8be7c60f690fcd1f9d062789";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "21095a96995398c93ed11ec29467af43d90e954df34184683b7a5e893aa7b289";

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
  "13da4f573a9c09e99ac7a8ea11a7e2d4709f2028ae739ed4a9adac67120879e5";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "680833c6142880f2cf59c1e945b0f6ce8db9eb8776d1cfdaadf269e960360784";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "13da4f573a9c09e99ac7a8ea11a7e2d4709f2028ae739ed4a9adac67120879e5";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "680833c6142880f2cf59c1e945b0f6ce8db9eb8776d1cfdaadf269e960360784";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "0bd225b09a0577cd221995bfa584ec0061a3e0395379b905d041c07429f4914d";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "b4944c4c1842996e78d365e8758665aa58658bd542596106f1cb7e00d1c4ab3a";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "7133c5d482404e7659c11198c92806a6ababb31e7175c82ff24db88870b59616";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "572469ccd2221edaafd1d27ab79c11c5cebb67de56ba1a1c2f2abb066cedd358";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "7133c5d482404e7659c11198c92806a6ababb31e7175c82ff24db88870b59616";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "572469ccd2221edaafd1d27ab79c11c5cebb67de56ba1a1c2f2abb066cedd358";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "9b0cbff821bc00bf28721cdf766e9bfa541a1c15e9d6186093b3d3f8b63874e5";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "88a588b00b6db220a7a61dd38609e0232b7d839ba96a353a9ed3563d1dee4298";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "324a3d877824deb85e0c1c3ce43b96801e484628b3f2b969331da1c13abc2975";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "d11125c8f39a75d57bc140f1fc957cf1ffbc3456f43f9e73f538f5f487dd10cc";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "324a3d877824deb85e0c1c3ce43b96801e484628b3f2b969331da1c13abc2975";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "d11125c8f39a75d57bc140f1fc957cf1ffbc3456f43f9e73f538f5f487dd10cc";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "445c385060650a3b5f4e284a3416d8f03aa440b41e1fdc6da99b9c2d731bb7b1";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "80445de3e731c06843653f79eefd9859914444dff559ba498e7d5e55fb7075a4";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "303ca5cf07ae0ab99ae07d051abbd353f8493d4fcd9e41ab4d094cc07515ae12";
export const REAL_COMMAND_ORDER_DIGEST =
  "d55ee73f78ac468ff7fddb77a35d16ea724a0995500c2850bd9ad5b0ebf26b3c";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "303ca5cf07ae0ab99ae07d051abbd353f8493d4fcd9e41ab4d094cc07515ae12";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "d55ee73f78ac468ff7fddb77a35d16ea724a0995500c2850bd9ad5b0ebf26b3c";
