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
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "affe030374fd49aad9f0bd0aa97af462adc5f9ddf8653604365f2a907996ab50";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "f46da6e89b046badfaa07e9505eca50177484973ddd4581f68872fbb59742f59";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "33e79d64e831e2660ddd975e13834018c37a4b2ea2673574234f3a522ad762fd";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "f46da6e89b046badfaa07e9505eca50177484973ddd4581f68872fbb59742f59";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "33e79d64e831e2660ddd975e13834018c37a4b2ea2673574234f3a522ad762fd";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "4f6ce9a3d2e5311dbe1304224357af6a74c13d4ae85f188fad569e2abd5d505a";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "85a843c67e9bb10850f7d50a5066bc31bb9fddeccaa77737ae4a66d04cb1b9b2";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "daac1a2f1ee370a2cc735975e8086a04a54e8171f9abf93f7c45e8407c3d6563";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "85a843c67e9bb10850f7d50a5066bc31bb9fddeccaa77737ae4a66d04cb1b9b2";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "daac1a2f1ee370a2cc735975e8086a04a54e8171f9abf93f7c45e8407c3d6563";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "3054885ebbf504ad71dc7f8a118eca130cbdfc82dc55f31308091bc5d2e3f8ce";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "d33d61e61945278cdf210942fd46f4af13b613be44ccd059104ae5cc680f48b3";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "760e33817c85077062c728f8782bd15e4774c33027002e3a6023c77b62045b1c";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "31de9984df574cffec068a0160a685a651abe36d601cfbe165405ab3c33ffb82";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "760e33817c85077062c728f8782bd15e4774c33027002e3a6023c77b62045b1c";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "31de9984df574cffec068a0160a685a651abe36d601cfbe165405ab3c33ffb82";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "cc8275008f7110e9d1ae728f182aea6164126fdeb9bc377ede1ae69cfcc3ed20";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "301bc74ec32aa6baabad28929c038d4e9316cabbbbf8db7ca832002f4e6bbd90";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "72e9a2f65edfefa91c0b8f954b312e8a60e46d9d490ab22019e292a63acdb96d";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "34c7b8eb159a0a9dbd0ca96ca85c5f6117c1a1170019c428a9b90b1c6caef10c";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "72e9a2f65edfefa91c0b8f954b312e8a60e46d9d490ab22019e292a63acdb96d";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "34c7b8eb159a0a9dbd0ca96ca85c5f6117c1a1170019c428a9b90b1c6caef10c";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "1152e6765c7a4b60e331c7157fe2f3f00af2db3d8ae4050436962ad0d33cdf83";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "2c972e6605bbf543c8658f36f0b1dc0057094a6e5c41cd201b146f5d2b0f1643";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "e2a23c240a997aada2bdb8290a6c60c59317b4394745f0bf69dd18c73598c474";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "610f885d6067a7a0113f40e9e8bfc3fcfad5b06d5de4b5b0a5ae560bb9fe6f2b";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "e2a23c240a997aada2bdb8290a6c60c59317b4394745f0bf69dd18c73598c474";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "610f885d6067a7a0113f40e9e8bfc3fcfad5b06d5de4b5b0a5ae560bb9fe6f2b";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "f7067109f45341b67b94ef2845ebac46009b1eccb7a539e706b47a1c0684e940";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "0228e23d0c90ce7afe6246e3f358709d5738c8e1fb656e4470afabfe26e3562e";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "d98c607b88199be6eb023af57543c986e031cb285f118817fb7d54315b720845";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "191777bbf892280faf82d189723dcae2290a868545d5813908f4c0e03a12ce2c";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "d98c607b88199be6eb023af57543c986e031cb285f118817fb7d54315b720845";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "191777bbf892280faf82d189723dcae2290a868545d5813908f4c0e03a12ce2c";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "9be37563b6235923f08efd03d3d73f837f4f37e7940381fa8255e884836744f0";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "d86384adb5fdcc0f2c6e3be5ee85ff5972ae0cda3bdd8f864077dd798ba73433";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "ad82b818452653153e63611d83c65a696b799c43690bfb710967347d87c1800f";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "f6c95d622ed944a145eedbb7833033fae4612b242f99c6c8ee5aa94e95f73b3e";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "ad82b818452653153e63611d83c65a696b799c43690bfb710967347d87c1800f";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "f6c95d622ed944a145eedbb7833033fae4612b242f99c6c8ee5aa94e95f73b3e";

// The CEK core's five staged-operand semantics steps, its staged failure hop
// and its writeBits loop add seven manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-staged pin and
// the 576/571 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "b981a245a20be807f2538a25e8a7a108663cb58c17debb47da75ee9928a7ac4a";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "4d9e7cf875ed76d7e4552114cbf42e202366379f11aaa3067bcedc71f8b4b365";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "dd2b789c3d87efb19033a127892dca0731b5ecce05043450da33726962a10ef1";
export const REAL_COMMAND_ORDER_DIGEST =
  "9d349c32079dfe5e1a628c2e8c7c4f7e8db70a19538682ed80e575f67f08d005";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "dd2b789c3d87efb19033a127892dca0731b5ecce05043450da33726962a10ef1";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "9d349c32079dfe5e1a628c2e8c7c4f7e8db70a19538682ed80e575f67f08d005";
