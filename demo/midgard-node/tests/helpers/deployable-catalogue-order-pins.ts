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
// They were re-derived once more when the execution-source-script-decoding
// family shrank from its five step contracts and roles to three, and the
// execution-native-script-invalid family grew from its 13 to the 28 of its
// direct all-purposes walk. The two families keep their 18 previous
// positions and append 13 entries. Removing the appended entries and
// restoring the previous names and roles at the kept positions reproduced
// every previous digest and count on both sides, and each count is the
// previous count plus 13.
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "a5661c2a40371880adcfc0e7bbf23add6d8490169b9f34ae9c93eed79582f32c";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "9b0b30cfa0ae39fce4fd4c637739e46471fcc0819a004aeae0f1aeba82713895";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "bb61a4d5d5ca1dbc711086f9f648edc741b78037aef350ea153241be3660878c";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "9b0b30cfa0ae39fce4fd4c637739e46471fcc0819a004aeae0f1aeba82713895";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "bb61a4d5d5ca1dbc711086f9f648edc741b78037aef350ea153241be3660878c";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "599ed156e856d2184e70e391e122023870d5859a4bbf119112f6cbe9d6bb5318";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "ebfe3e1b9d001403e141428983ee8df79aecb8693ecb3b8d70ed82c6955826ff";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "094e3a5188b0c4a60ee743aff53da88c0f6b4dc8c2e96dba46ed0c5d25f3d6e6";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "ebfe3e1b9d001403e141428983ee8df79aecb8693ecb3b8d70ed82c6955826ff";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "094e3a5188b0c4a60ee743aff53da88c0f6b4dc8c2e96dba46ed0c5d25f3d6e6";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "5d91f4a90077a144d249c3802fe6ce9151f06ae33f75c3dfa0f9b4a94948ac06";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "bc7d378b00dc2e6bb4f04e82630d3ceb3fe21abd4a6eb6d78e40a074fe51e8e3";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "808be56331e7ea638675f2aaa234ad85995142e97e662a88d7351cfccd0e7985";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "2a4418535adcb3c6b86879a0f10b9aeaa5e7c38726f2a51e8e14b09ceec4d6bb";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "808be56331e7ea638675f2aaa234ad85995142e97e662a88d7351cfccd0e7985";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "2a4418535adcb3c6b86879a0f10b9aeaa5e7c38726f2a51e8e14b09ceec4d6bb";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "704ac9c85bd6cac4470d2751dca2f95f9eec6a0c3a0e2ee525b5f84bf9f6c16e";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "f5115a07dc068ed4a531539f518c67dd56257d854724f92ca4a4ae52f5f678af";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "cac818d8373910c713c0aaee4f74ad436cc843db9bf2ba07b15ef1c7c74b5bb2";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "d3c3ea736c57d55f2609e3e4536f84eefec0c4b953ecf74ed8ec3bf857a3825a";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "cac818d8373910c713c0aaee4f74ad436cc843db9bf2ba07b15ef1c7c74b5bb2";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "d3c3ea736c57d55f2609e3e4536f84eefec0c4b953ecf74ed8ec3bf857a3825a";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "f2cd929d3734c83968d835fb66b25084cf383f79e32108a2d32643d9390f4566";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "5751f0e16dd6e86e3014bac1e6d598fd21e98201070a38eed0ed6ae1d75a1484";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "c1b3616722c6eff733bb78275d716791b8305d8b49f114ae6127e94f7b2cbc39";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "3fd102ae1499baa4910e4244a4682e6dcdd6f71f9edc5963a0c76aaa2a4fdf28";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "c1b3616722c6eff733bb78275d716791b8305d8b49f114ae6127e94f7b2cbc39";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "3fd102ae1499baa4910e4244a4682e6dcdd6f71f9edc5963a0c76aaa2a4fdf28";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "647983d6fcc20dc10b576068a1659569cc2e776044d960f65b596f2221e33cbf";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "7c02739838163a0fbaf0c690381313a2a8ceb6b1fc773a3d560099551d35d35f";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "4faffe5af41b79a266d1e9a358646f867f148564d375c53073266be379166c91";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "032a96ad45150c289807f02b2b26b50a15d90f193f6f384ff1624c07887710f0";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "4faffe5af41b79a266d1e9a358646f867f148564d375c53073266be379166c91";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "032a96ad45150c289807f02b2b26b50a15d90f193f6f384ff1624c07887710f0";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "3305e4051f84b929cade4bb0e3498cae52b6e3b5a8188b1b810c960c6fc4c66f";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "a3cb67cd65ad0262031cdf8a72699fa25155663a6c8ae4df7c3166707f42acbd";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "87b15ea15875ffa7093dd5052c86450a432590d9e6a4e64fdeb46a3adfb281fa";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "75330ccfbf6063ddef2852727d235e8660e3ed090ef5ea0ffc4bb7666cbc6712";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "87b15ea15875ffa7093dd5052c86450a432590d9e6a4e64fdeb46a3adfb281fa";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "75330ccfbf6063ddef2852727d235e8660e3ed090ef5ea0ffc4bb7666cbc6712";

// The CEK core's five staged-operand semantics steps, its staged failure hop
// and its writeBits loop add seven manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-staged pin and
// the 576/571 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "63cad38587cd59fd18665dbb83636d2f2dc67c3c6b973347fb08bee7ec3150db";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "888e76ca5c09215d7376988af19fde7384fc8c11b9bbf5ad41745021f24977ba";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "5da551d2301d61b4b191aa2620a0d749feb0e97db1ad493fdd7b1917b4ec94f2";
export const REAL_COMMAND_ORDER_DIGEST =
  "a6805a3c5ccdb2a8211e93f2350df93fb5c0541c4efe6f9c793bbe776d0c894e";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "5da551d2301d61b4b191aa2620a0d749feb0e97db1ad493fdd7b1917b4ec94f2";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "a6805a3c5ccdb2a8211e93f2350df93fb5c0541c4efe6f9c793bbe776d0c894e";
