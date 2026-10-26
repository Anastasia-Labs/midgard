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
// They were re-derived once more when the script-integrity-hash-mismatch
// family grew from its five step contracts and roles to the 21 of its direct
// all-purposes walk. Collapsing its contiguous block back to the previous five
// entries, with their previous names and roles, reproduced every previous
// digest of every generation, and each count is the previous count plus 16.
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "80742ab6cf31fd40aea73e6782e839e6ac141d4bfd9c8fd04377ff7810f30d54";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "bbae78e9b664dcaac2af4d4ed2ee679806d8d54409ca9c90bb996f28fe89953f";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "6dbd81e03393cfe5e1e1c356a829c8837e9d11cfd20f643cce02960370f38bc0";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "bbae78e9b664dcaac2af4d4ed2ee679806d8d54409ca9c90bb996f28fe89953f";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "6dbd81e03393cfe5e1e1c356a829c8837e9d11cfd20f643cce02960370f38bc0";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "51764dc5e6e8a46390984f29ec05fb8c92a2b2f8160d473f4edfef8ff17180a2";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "66f89333caf4c427ad6510483e909b9666d9db9aaa549a3087725d027b5f6e35";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "73941306df085d6ee8326706cd49906f961ef24984b4f8cb401ed6357934918b";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "66f89333caf4c427ad6510483e909b9666d9db9aaa549a3087725d027b5f6e35";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "73941306df085d6ee8326706cd49906f961ef24984b4f8cb401ed6357934918b";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "fb25bed945f476846e4d794ad3329b7a8f02ce035ec35c27038d6b8040ca8a55";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "378d4153a559cb49711465ca16a21f390afb5f1ea23b8b33650318e3f2e71459";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "20960b7cd41f12de360d516d04b2f8af9d1aa7436bef2e01217b21fcbb5d0cb2";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "b9bf777e0cffa9709739f70ead5b5295158470038fc9dca773deb9341b9b9334";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "20960b7cd41f12de360d516d04b2f8af9d1aa7436bef2e01217b21fcbb5d0cb2";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "b9bf777e0cffa9709739f70ead5b5295158470038fc9dca773deb9341b9b9334";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "d95972706c6d7ec4c384a24865e944a76e289d1a08b249a5860b1b015ab1710f";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "70fbb8a6b6fa67ee8ca5e546000fccaa2a2c4f509f1c49367fedaf0bc4bfaaba";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "0c60165c33402166b530602b2586291938e5ffb8d42055caa10937977cbb15ba";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "66468c23eb861f86dc302c563f5d23fe3e6f1c30f6fcce4bc4c8d13436c4bd84";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "0c60165c33402166b530602b2586291938e5ffb8d42055caa10937977cbb15ba";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "66468c23eb861f86dc302c563f5d23fe3e6f1c30f6fcce4bc4c8d13436c4bd84";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "4404dd0f7e5a656029ba8b3dc949f0da265adbfeea63d6cae5eee72396d18409";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "b78f9fe2ebe9413e095a196ef432aeeddc1e26a7c590681ee3dfddf1218546ac";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "f022a1baf6a72a19285bbfb40bdb30f205cc4396cc94bdedf26bf35e100d15ff";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "4098e66bfba08f86c5ccea426c5841a0525b8db99e63210431cbf87199bb7126";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "f022a1baf6a72a19285bbfb40bdb30f205cc4396cc94bdedf26bf35e100d15ff";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "4098e66bfba08f86c5ccea426c5841a0525b8db99e63210431cbf87199bb7126";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "de6cde8b13d63436d881d35e23da65cd8aa5a6f0d6e338e6ad258574fac80c46";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "0e7e6fd67134333e2129fb914b1a37e11cdc16f55140f4a77afa2fd769761881";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "82b156420c26a0bbdb475cd58d6f970ae5a589f94150ea89f9e6bb6a138b4686";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "b3142c3f686676eff563d5fbc81695ce08895d106fd8e4539a9312a4b2c7fb4a";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "82b156420c26a0bbdb475cd58d6f970ae5a589f94150ea89f9e6bb6a138b4686";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "b3142c3f686676eff563d5fbc81695ce08895d106fd8e4539a9312a4b2c7fb4a";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "9aa39bb037ca3784723b649e4816938f27b07998488ea73880e975538cb6251e";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "fa8052cd5d0f612c13ba0042107703c6996c386598c71007c5e2f559cabb94f2";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "2c2fa6c10886f62b354b0dbe6cd631facf89f288ad4b15fa6d0c002346fc5f3e";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "e3add6cf26760a0559efe5b640bef1eca5899e13c9e8a9a62bb1918dfc76fe24";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "2c2fa6c10886f62b354b0dbe6cd631facf89f288ad4b15fa6d0c002346fc5f3e";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "e3add6cf26760a0559efe5b640bef1eca5899e13c9e8a9a62bb1918dfc76fe24";

// The CEK core's five staged-operand semantics steps, its staged failure hop
// and its writeBits loop add seven manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-staged pin and
// the 576/571 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "90e5602e82ff01558a1d15946443c5096ffa667dd8dd8ed33e3c37d6cb0fbb66";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "5ad4eef0af54bdf2ea47f7c73219e488769ccd5add7c6da336dfc4510f78360c";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "e80fb2fc11087127db11062ac741b4f8183f0c88b785671f12e2ea5c9aa32f6a";
export const REAL_COMMAND_ORDER_DIGEST =
  "a6e8841598d970e250ad5a365669b3cd8ce34721f5c75f2cc17d21e2b0594a0e";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "e80fb2fc11087127db11062ac741b4f8183f0c88b785671f12e2ea5c9aa32f6a";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "a6e8841598d970e250ad5a365669b3cd8ce34721f5c75f2cc17d21e2b0594a0e";
