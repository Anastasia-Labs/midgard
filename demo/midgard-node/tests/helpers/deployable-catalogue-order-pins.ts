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
// They were re-derived once more when the distinct-asset-accumulation-limit
// family grew from its six step contracts and roles to the twelve of its
// direct walk. Renaming its first six entries back to the six steps whose
// slots they keep, and removing the six appended after the last member,
// reproduced every previous digest of every generation on both sides, and
// each count is the previous count plus 6.
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "0ad3ff08b4736128028b8c2665d867b0fec8c6abab2aac45412fc3bf6dedbd32";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "63a9bc2b22e5882a2e2054c66f47bff69050275b09c457cc411e6f83f911467d";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "a6c6681cab5ce8344091756118c3048340dedef2ba9a4057df3d9ac5824730e6";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "63a9bc2b22e5882a2e2054c66f47bff69050275b09c457cc411e6f83f911467d";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "a6c6681cab5ce8344091756118c3048340dedef2ba9a4057df3d9ac5824730e6";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "95bd1aab32d36266cefcad0a0c6a75783d925f5a43b0006acd76951dcd8aaf08";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "54e2cc4b0c167003b83ad3100d348b477c05edf3e2df4891cca548e89a1999b1";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "7e2dafab0c6068272e052b5f19427f0aad460ca59e54e42f3afec5a61d5c7bce";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "54e2cc4b0c167003b83ad3100d348b477c05edf3e2df4891cca548e89a1999b1";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "7e2dafab0c6068272e052b5f19427f0aad460ca59e54e42f3afec5a61d5c7bce";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "8e5f30ed4da924f959b9cdf30bac3b5f1166f1d92bd90f08adc263e622940eff";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "642cc6b91cbf77bac500332db597bb89c10d0f53ec940ca8975759e81985f5d7";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "038d653dc9bd6199cf977c19326580158506df17c666f0e66f567eb646123a28";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "b7ec8d34ea699c4a75ebc6b0e33a5b23be00ba6894e201841e8e2af75c35a7f3";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "038d653dc9bd6199cf977c19326580158506df17c666f0e66f567eb646123a28";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "b7ec8d34ea699c4a75ebc6b0e33a5b23be00ba6894e201841e8e2af75c35a7f3";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "3c74426d408deecb98089e75a11738449f338cbd49e9a3ab4fa12109205edb1f";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "bfbc56fe9dc8d5076009be4e09f9c2ab58fc81bb5f97e791c55c8f036f2a46c4";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "72ead23b4248435bf8e919663043a81e60ed0cd7631ba902193f63ec20ca0a62";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "66d3e91317818a685659e2c64399e8ae9bbbd57deddf78e37804dc383d0f0166";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "72ead23b4248435bf8e919663043a81e60ed0cd7631ba902193f63ec20ca0a62";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "66d3e91317818a685659e2c64399e8ae9bbbd57deddf78e37804dc383d0f0166";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "29ebb81dc0c68c5687cfde048088b3408857447dd652b20b6903cbce92ecc930";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "ea257e0791c840526c73d118830e5368b7b97328ce6161e38b0bde35dc441994";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "26fbc515181776a50fc84ecfcf1962fc2fcedd7e92046ce37fbab5af35139f08";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "217ad5ff314c80c65cc6b5563dcfd7e59aed22d1b52bff604e005a75be041fde";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "26fbc515181776a50fc84ecfcf1962fc2fcedd7e92046ce37fbab5af35139f08";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "217ad5ff314c80c65cc6b5563dcfd7e59aed22d1b52bff604e005a75be041fde";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "43d10b7b8dfb7d6bd6308bc983e561df15ceaa8bddc870b585accf4333af09fb";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "aa0036297263a46500357408d7c860f1f242ca756a541a13b7bc9e790d1ce169";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "b7c262cf95074e52e68502dc46003d412476ae67b6233f4cebdc817888d46395";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "24274e4b8a5619a50a571a8822084188fadca639533ae128dc35857d1828819f";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "b7c262cf95074e52e68502dc46003d412476ae67b6233f4cebdc817888d46395";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "24274e4b8a5619a50a571a8822084188fadca639533ae128dc35857d1828819f";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "84f5eb34c68c38b3668d5d23232c07e8d5e8cff11d95f0d18cb8bd257ed1b069";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "0816e39118f5863dba04f1d4f1079dfdfa2b2678f1d0db6bef0f6c435172daa4";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "e2e7d21c466bbf862c6c08e70ce94cfe463145be2fe78745855afa5c11903f92";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "dbb73c6ec44a3e2eb7ca40c00dd273433bc574c78227e3ada7262d9846ef4917";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "e2e7d21c466bbf862c6c08e70ce94cfe463145be2fe78745855afa5c11903f92";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "dbb73c6ec44a3e2eb7ca40c00dd273433bc574c78227e3ada7262d9846ef4917";

// The CEK core's five staged-operand semantics steps, its staged failure hop
// and its writeBits loop add seven manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-staged pin and
// the 576/571 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "1cb48afc1316d9eee1d5ba855acc843054c5f7c6fe3336da22f96180c3a031c3";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "33607a6d76cbdb86a25b3f8e51836f70e4a2ffd29da95ada35cbcdd3c2fce445";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "dc17e3560c7bb7fa6491dd18d90a4654800be10de3d7ba55d7f0ade83ffb54ea";
export const REAL_COMMAND_ORDER_DIGEST =
  "764acf1440e15d52d8b41f3cb0bd435a3cf3658d4c8cabf03d4b7abf7c536cea";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "dc17e3560c7bb7fa6491dd18d90a4654800be10de3d7ba55d7f0ade83ffb54ea";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "764acf1440e15d52d8b41f3cb0bd435a3cf3658d4c8cabf03d4b7abf7c536cea";
