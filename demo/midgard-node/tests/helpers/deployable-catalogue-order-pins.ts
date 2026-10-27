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
  "3a96e0b2f9921812999ead9a933cf8c456bad5e22804a9d34b0f87ed52ea2fb9";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "f5e67bc41531c75086ecb8817d1d5c0c97fdcc47f70cac003e35e819ba423ab2";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "d56fa2dedc6a82054d74f1e794aa175d7ac870de896083d7dbabbd2bf1855146";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "f5e67bc41531c75086ecb8817d1d5c0c97fdcc47f70cac003e35e819ba423ab2";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "d56fa2dedc6a82054d74f1e794aa175d7ac870de896083d7dbabbd2bf1855146";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "cf85e0aa37478c37bd61cdb902733d5b99c07ef3f16b05ed2379d3097bdde68a";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "8aa0d95ef7154d477ca5343f2d80a2d67ef04e1f7e0259a8b62c4bb1fe5c864d";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "9ff9754cdf9d4e06078287b53bb1729c22c89622733b968a11b10d9b95f0a990";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "8aa0d95ef7154d477ca5343f2d80a2d67ef04e1f7e0259a8b62c4bb1fe5c864d";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "9ff9754cdf9d4e06078287b53bb1729c22c89622733b968a11b10d9b95f0a990";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "13b3f1466233ca5f178ae8ccb0053b648dbf60485343e4f37165d295926f8e7e";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "ef91a6bf5c563583fc108f9b98e39e3ee17029ea9f47620d756703d67e50f9c0";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "2984ed8e629ac75373140dbcb68565ef171c5633f4002d998d8d5062301d94ce";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "e377df84f6a8e7c952c3e908f0b03cb5675f6b73b6f87afad5a8e366caab4268";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "2984ed8e629ac75373140dbcb68565ef171c5633f4002d998d8d5062301d94ce";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "e377df84f6a8e7c952c3e908f0b03cb5675f6b73b6f87afad5a8e366caab4268";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "0802b5d989d8bc0e04abc7ca955ff0cb465412652078efe0342a90c14c591b2a";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "a0d5aa3df48e4879b804f9dc4c46665d10c54327236457ef7c31c9d43dc7a05b";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "63bf0d020f3a3d6da64291a0e9a28cc4f0f16bd3932e625d2cb32d5f2eb05de3";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "31e67b39286c4b40fb2623881de57a7c3b5a7f002b8e04f5b3cc396fcc34cc0e";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "63bf0d020f3a3d6da64291a0e9a28cc4f0f16bd3932e625d2cb32d5f2eb05de3";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "31e67b39286c4b40fb2623881de57a7c3b5a7f002b8e04f5b3cc396fcc34cc0e";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "e0909d5ead7505f709e80859955f03f047e444bb2c286fb447df616c29c548ef";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "c3fbe2fec6085d7e497f86e8cb1697ebf7407eff723600715864f0e675a0f5c5";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "58cc0ac3e74ff9db7d1f7774a2040e521a35f8d4263830926f44ab432dc88347";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "929ae33f1244475a2ee214233f1b727c685b88800ec6fe9d3830f6b41e3b2312";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "58cc0ac3e74ff9db7d1f7774a2040e521a35f8d4263830926f44ab432dc88347";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "929ae33f1244475a2ee214233f1b727c685b88800ec6fe9d3830f6b41e3b2312";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "b7e05977ce99b0308a7c8cee827f15bf9c6e8838b9593ac26181dbdafdac55b5";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "181ca42a5de7e8b977b55bee9acc4f955ae9329c05c61d00b3990481a4221492";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "cbbce3ec9e16e7fcbe5cd8f4a7ebd2fc11f3b3f29a198e979a0cb2044bcde45e";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "25eca933bebdd3a9ad73ecd8ed5d6202ea9b0c780de5f53c63b632d2ce544687";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "cbbce3ec9e16e7fcbe5cd8f4a7ebd2fc11f3b3f29a198e979a0cb2044bcde45e";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "25eca933bebdd3a9ad73ecd8ed5d6202ea9b0c780de5f53c63b632d2ce544687";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "400ab94f54ba6a086d70246d5e3d637bbba35841931495865a5e1540f6a82afb";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "c5b6a8a015a5bc51abac7c69c56f9f66c3a5f36c2fd3b37176833f49b7657308";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "8f92bca86fe2a5afec8b7d2fdae0451eb70c4147df033ee9790301e0294dd784";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "6a7d7b56f4f6399cac7e14db84cff8aa6d4cc16b2c233c04ba382dae578aeef9";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "8f92bca86fe2a5afec8b7d2fdae0451eb70c4147df033ee9790301e0294dd784";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "6a7d7b56f4f6399cac7e14db84cff8aa6d4cc16b2c233c04ba382dae578aeef9";

// The CEK core's five staged-operand semantics steps, its staged failure hop,
// its writeBits loop and its BLS12-381 uncompress failure arm add eight
// manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-staged pin and
// the 576/571 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "a6b46c6356f66af6d1de9ddcb3e53e8ea58732b5c950e48f099acc7ab30da444";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "eba002b8e50222ab7242ab80e191cd63104d0b5c806c8a2a877612b8b5aea9a7";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "7ce463c255a1857cc24151d22dfcdc86c7f9079b908f4f57b02d46718bdb5b66";
export const REAL_COMMAND_ORDER_DIGEST =
  "7ff7532f28816391cc82b8981c021a8a952fbf525957b5ba2c3a266f8cc2137f";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "7ce463c255a1857cc24151d22dfcdc86c7f9079b908f4f57b02d46718bdb5b66";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "7ff7532f28816391cc82b8981c021a8a952fbf525957b5ba2c3a266f8cc2137f";
