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
// They were re-derived once more when the receive-purpose-language family
// grew from its three step contracts and roles to the 21 of its direct
// receive walk. Its first three entries keep their positions and the other 18
// follow the catalogue's last entries. Renaming those three back to the
// previous names and roles and removing the 18 reproduced every previous
// digest and count of every generation on both sides, and each count is the
// previous count plus 18.
//
// They were re-derived once more when the distinct-asset-accumulation-limit
// family grew from its six step contracts and roles to the twelve of its
// direct walk. Renaming its first six entries back to the six steps whose
// slots they keep, and removing the six appended after the last member,
// reproduced every previous digest of every generation on both sides, and
// each count is the previous count plus 6.
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "bcdafb95d8f551a40b4201fdcd22334e171d6857f3325334b37653ff31cbdb11";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "380a131fb56dfad49c7a4aa322eab35f101bfcc344281747b570f25b34300a45";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "21d6306714345eba4eceb5bfe33ea7348f3261b82d509a0964c812dfe7083800";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "380a131fb56dfad49c7a4aa322eab35f101bfcc344281747b570f25b34300a45";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "21d6306714345eba4eceb5bfe33ea7348f3261b82d509a0964c812dfe7083800";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "5b3e06ce0ce8a1ac0fc5eebd4fd85e68cbb4240909a819b5ce2ed9183e018ebc";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "027612b293e486dec5a567dc8abe0e3443667190bfdd153636c0fefc1904a863";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "96c29d1540bf334cf05a9068d7886d9c5aa26999913e5d0476749eac204d9942";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "027612b293e486dec5a567dc8abe0e3443667190bfdd153636c0fefc1904a863";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "96c29d1540bf334cf05a9068d7886d9c5aa26999913e5d0476749eac204d9942";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "d785a3834f0295153730983f44c609508f03d7d210c7358ba96721ff06128a2f";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "e89df38420050f3721a608a1c4b7e371ed87846844bfd21e0e6473e7ae077a0b";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "56ebed4e39aabc2a9c190e1b1380ef6c03f3f32fe117e239d2471bdc3021bfd2";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "6c27edd6cd88d1e126bddda8371b88d169bab040a4a6458ec8eb3013c8d71046";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "56ebed4e39aabc2a9c190e1b1380ef6c03f3f32fe117e239d2471bdc3021bfd2";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "6c27edd6cd88d1e126bddda8371b88d169bab040a4a6458ec8eb3013c8d71046";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "4989b90dc2329ff73df2622030acba735d43ff4e6d3261038d10541f19f14f17";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "66ba988dcd8bf174b676e506ebf4347e058bf9d7313dbaf85e69310ae15f6a70";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "c8b5002b6127726b79b6fb6349abc3545421fb5e4bef3a3b86a8434612649d44";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "e0ce21d6831df34b3b7e86c64b7e4c7a16ae3e9773d3a0b35586dda247af0da6";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "c8b5002b6127726b79b6fb6349abc3545421fb5e4bef3a3b86a8434612649d44";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "e0ce21d6831df34b3b7e86c64b7e4c7a16ae3e9773d3a0b35586dda247af0da6";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "7d54ada38f1437d0acac0c647a1e0ff2d2bbbff451196bcac568838da96a8fb6";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "36c00b18024ea52b886a736f2fccdacd10e257cffc9e85939b8698a2bd5c7da2";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "fd27dd95b07e393c8babf912291fb02639ff469c29699e2b4274e6a3da00c5c8";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "b1d1189ba49fd07075f1b96d8e2f48f2a4991ef211a50391a50a12c556a5f359";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "fd27dd95b07e393c8babf912291fb02639ff469c29699e2b4274e6a3da00c5c8";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "b1d1189ba49fd07075f1b96d8e2f48f2a4991ef211a50391a50a12c556a5f359";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "b7769f340ee77db00e0b2d0663465e171d08a107397ce66156f55e2a92bfc6f5";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "4eafa866bc474c04bd41762a31d8d44a5a16f45637bfaf86c1504deb64babaa6";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "c2b009f88d6b0c8f45698b6dfcd83cf197634643fec90c3b173cf9f72676ec0a";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "ecde3d470cbb1330042d397903fa818a5d70ff90aac05176537b1ef027233399";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "c2b009f88d6b0c8f45698b6dfcd83cf197634643fec90c3b173cf9f72676ec0a";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "ecde3d470cbb1330042d397903fa818a5d70ff90aac05176537b1ef027233399";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "256ebf04ab8420a788d1228573752041c543559d756a497e6fe3f1ba5a2f34f8";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "7954f4ded6b04e957051d97e27071574cef1a5dfd6f633c1088031a5a9c88251";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "c20b0115fd15a29bacf1404624e0bbbaa1723106631743185db1422fe868ca58";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "6696998eeb17b5a5bbbc821ca1c125d9dc76c241faac3f7d063692fd16efc93a";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "c20b0115fd15a29bacf1404624e0bbbaa1723106631743185db1422fe868ca58";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "6696998eeb17b5a5bbbc821ca1c125d9dc76c241faac3f7d063692fd16efc93a";

// The CEK core's five staged-operand semantics steps, its staged failure hop,
// its writeBits loop, its BLS12-381 uncompress failure arm and its three
// protocol version 11 arms add eleven manifest entries, each a published
// role.
// Projecting out exactly those entries reproduces every pre-staged pin and
// the 576/571 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "ceec0d223e9d95de26458b164a2dc5bec0fa27ad3f09add7f56b2df7e9e5f829";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "ce3cc04f97e436947188b6bb6b25b6e50c8d6ea2cb1199c213812ab8c7fdab01";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "bf551b37a94e9bf4ac48d5077e4920284bf9eda3c14f7f39b268dca5e028a945";
export const REAL_COMMAND_ORDER_DIGEST =
  "8a71db6bcb164ccdd13197a7e59f1565c3157beede39df289a06b9912ce0c0dd";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "bf551b37a94e9bf4ac48d5077e4920284bf9eda3c14f7f39b268dca5e028a945";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "8a71db6bcb164ccdd13197a7e59f1565c3157beede39df289a06b9912ce0c0dd";
