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
//
// They were re-derived once more when script-decode gained its six
// control-class stakers and its spend and mint rejoined the publication
// orders. Removing the six staker entries from the manifest, and those six
// with the spend and mint from the publication and node-runtime command
// orders, reproduced every previous digest and count of every generation.
//
// They were re-derived once more when the missing-redeemer family grew from
// its seven step contracts and roles to the 39 of the direct-derivation graph.
// Collapsing its contiguous block back to the previous seven entries, with
// their previous names and roles, reproduced every previous digest of every
// generation, and each count is the previous count plus 32.
//
// They were re-derived once more when the missing-script-source family grew
// from its six step contracts and roles to 37, and the unused-redeemer family
// from its nine to 25, both on the direct-derivation graph. Collapsing each
// contiguous block back to its previous entries, with their previous names
// and roles, reproduced every previous digest and count of every generation,
// and each count is the previous count plus 47.
//
// They were re-derived once more when the script-integrity-hash-mismatch
// family grew from its five step contracts and roles to the 21 of its direct
// all-purposes walk, alongside the two families above. Collapsing each grown
// family's contiguous block back to its previous entries, with their previous
// names and roles, reproduced every previous digest and count of every
// generation on both sides, and each count is the previous count plus 63.
//
// They were re-derived once more when the unused-script-witness family grew
// from its six step contracts and roles to the 24 of its direct walk. Its
// first six entries kept their positions under their new names and roles, and
// the other 18 were appended. Removing those 18 and restoring the six
// previous names and roles reproduced every previous digest and count of
// every generation, and each count is the previous count plus 18.
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "109818384d2bc96499c566c8a83e4d4b8f677baa758dfdf188d657542d52eae4";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "5d2536c04d8e4696e432868c08a88d19960e9d54d74d6f9ee3b2db9328fc3b2e";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "7b770405ce01dae1943ae04917ce1535cbc4bd4f6c3f20d74b9cf3d751d750be";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "5d2536c04d8e4696e432868c08a88d19960e9d54d74d6f9ee3b2db9328fc3b2e";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "7b770405ce01dae1943ae04917ce1535cbc4bd4f6c3f20d74b9cf3d751d750be";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "1888c0794ce64cf34353d5be10df8fa887f036d4fe4aac611d4073db21fe42ad";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "f9879fff0586ef941569eaca0971d323c32b50d2595eff312d8723fb75bf83d6";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "c76451569696b710621488a48adb338b86cac6bce4f151e402820a96d0d94f89";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "f9879fff0586ef941569eaca0971d323c32b50d2595eff312d8723fb75bf83d6";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "c76451569696b710621488a48adb338b86cac6bce4f151e402820a96d0d94f89";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "1db0b9c94c7f33cdedc5ac2de96c2d636a277028019fb6123b521ab67c9c257e";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "e77e5f587c4609b20437651a6465ee5086adbbd5a4844d2d56eaea403557e617";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "bcae3757adf645f71cd0da3e0a845c07edf2d13002f7ade64016a1a88e935133";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "1e6b84f054a862199f63e58285bf137f230d5f6ff9cbf192c32b71f7417219ad";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "bcae3757adf645f71cd0da3e0a845c07edf2d13002f7ade64016a1a88e935133";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "1e6b84f054a862199f63e58285bf137f230d5f6ff9cbf192c32b71f7417219ad";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "3cd26d96ccdffae455668f533424b350ff6dafe992c67379be013b17cff6d74c";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "b97040adfb963d1f42224e68990296a2fd95d664a012c16e273d92ab2a69a1e4";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "81c4f140acef7af4282d392f0d22e52134435cb27a3b03893820697cd1f03c6a";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "ee901da1e1a28eeb3a3fda55f05405c208a6c751330da36270e933852aa1d7d6";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "81c4f140acef7af4282d392f0d22e52134435cb27a3b03893820697cd1f03c6a";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "ee901da1e1a28eeb3a3fda55f05405c208a6c751330da36270e933852aa1d7d6";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "345a71016b5b64899218e58aaf7a7d5163d8943e18610184a8fd4f2151784c82";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "32cda7f674fd82cd2daa123ede761c63d03a09db609f21fae9c4483d0d050537";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "2748af52323ff0167754c0baa6f6ade7b8ec0df1177d8296dda76a3eb1e24ec0";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "c37433f959de27c16f7437645cb94706c88047dbdf272c88c6669e210db20b21";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "2748af52323ff0167754c0baa6f6ade7b8ec0df1177d8296dda76a3eb1e24ec0";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "c37433f959de27c16f7437645cb94706c88047dbdf272c88c6669e210db20b21";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "4752e12743e8e0a8cef367db5870bb99f36ba0822708ada0e44e53b6e3e75b6a";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "8f3e0b9617c4a2b3d99cf5ed3ca14c97a5ab2ccc1e7f70da23c5498fc6c82627";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "b5358303a60d9022940caaf05abd6e49f95f6d5579f052e3523b9bd634251565";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "3271a3644b574ea635e1a212d02b2324fe0177278b0c7a89ed9fe9ec85e7a7f2";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "b5358303a60d9022940caaf05abd6e49f95f6d5579f052e3523b9bd634251565";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "3271a3644b574ea635e1a212d02b2324fe0177278b0c7a89ed9fe9ec85e7a7f2";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "44f215875e471a3e416541c4734015c7772e21d753a15b9a83b3ef30d5e3b9bb";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "2218b24ea53b01ec8d1d4a5f0179a7eb9175fa5d6d207bc75cf487c62c415e75";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "c89bc130e695c991c2f67aa0bec90465fcd6cf0c6853944ba100287d5fad9c00";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "3b6070b585aa659a64457da00d10dca93b645b26221df6162dce50de03b9d8a1";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "c89bc130e695c991c2f67aa0bec90465fcd6cf0c6853944ba100287d5fad9c00";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "3b6070b585aa659a64457da00d10dca93b645b26221df6162dce50de03b9d8a1";

// The CEK core's five staged-operand semantics steps, its staged failure hop
// and its writeBits loop add seven manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-staged pin and
// the 576/571 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "20aa90c531494bcb7b17a64deb535c0887b9450ef566db28569c2ae76e819b14";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "13480b8bb18d35e8bb027d3d8753d02e2e5a16087638c16a5a2aac56d2e291f6";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "a73262145580b93b6f0dcdcae58524169729e83a25dac8bd6381a2b540695ec0";
export const REAL_COMMAND_ORDER_DIGEST =
  "63aa2f1d0bc146a75cc2ef9fe61558b59978c11c1deffa23781ad6b1b580a951";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "a73262145580b93b6f0dcdcae58524169729e83a25dac8bd6381a2b540695ec0";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "63aa2f1d0bc146a75cc2ef9fe61558b59978c11c1deffa23781ad6b1b580a951";
