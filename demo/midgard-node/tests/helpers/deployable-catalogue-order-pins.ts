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
  "d102c9dd7c96172a03ed469953600beb2e12d4d70e0cdca630ef8758e518bc09";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "22e7edc90e549c2a7addd622409dffa5a413c4235c99ee8e1fb742e83925b663";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "7bab407a2fa4aeedf7b641a81a87ff50b443ada6f9d3b69ac7d5e8a9b46d5b77";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "22e7edc90e549c2a7addd622409dffa5a413c4235c99ee8e1fb742e83925b663";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "7bab407a2fa4aeedf7b641a81a87ff50b443ada6f9d3b69ac7d5e8a9b46d5b77";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "a5f5c8afd46b8690c8954f92c1508c33254926372e1c9c2cbdc2566fe83dc4bf";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "ab8621c25df04fd805f51472057edbddb49bf8e85cf7a839cf97db6542e3e06c";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "502b9aed22f4235618840b949c43c6b7255225252d8c4c160bf7014dad203757";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "ab8621c25df04fd805f51472057edbddb49bf8e85cf7a839cf97db6542e3e06c";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "502b9aed22f4235618840b949c43c6b7255225252d8c4c160bf7014dad203757";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "ed15972a407fb4e7595290f2198ae345ebc03f205ebc116c88660b5ecc954715";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "de890be16c67ad0c69ad8ccdc0091cf0a495ee5476cff400167a10e681e38e97";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "46c7f7b8af566d04b5b2bc77c559034ae4305c99eccb9c26b3223154afa59ff7";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "88d47a4db5bcc92506f518320543518ab7442a76cab349de96f2380317723835";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "46c7f7b8af566d04b5b2bc77c559034ae4305c99eccb9c26b3223154afa59ff7";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "88d47a4db5bcc92506f518320543518ab7442a76cab349de96f2380317723835";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "7c728e73a0e89ff7bfa40d45a351ed87ef32a3d413948d43c3df1336c3f12647";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "70b1ed046993382719bded7fd2ca512d4afa492b9c8da0904b2c424258edfbb0";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "37c933e693949651ef6e83e8eebdfd43c4b9120202c92d60b89948af09aab921";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "6ca2ad3cb0e5b515445387cd86abf9ab280296771beb1e8906a16a47faf85eda";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "37c933e693949651ef6e83e8eebdfd43c4b9120202c92d60b89948af09aab921";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "6ca2ad3cb0e5b515445387cd86abf9ab280296771beb1e8906a16a47faf85eda";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "c5cc989555a4f7b7ad1da9fbb31bd4ab5218afadaa52d4eea1c281e1062acf40";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "75ed7f8dc719d487613c13546e4ec2bdc00d87083933005005abd8e83f8b106f";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "f27242f2c4c1e52f46938306ad3f63d76a6a0024c5e5899b2665b79838187821";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "128cc8f7a9ed4ce84f07d697be2e37b758d800dca8fc53baaa536a4babadafea";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "f27242f2c4c1e52f46938306ad3f63d76a6a0024c5e5899b2665b79838187821";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "128cc8f7a9ed4ce84f07d697be2e37b758d800dca8fc53baaa536a4babadafea";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "bec032e0d1891c5aaf29f69957cdfd8ec188762433f9a4432b032729785abc67";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "bf532a378589106d15ec721baf848d59e16d65863db8fcc03883f67512ff5b46";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "c38ed2c26ec4e242a0f4f280a520dfc3640e95101d301657c19a711ae0635d66";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "ff2d2bccfc4933081fb9ea663e0733b420eebcf37fb9ca22a5fa7fb57b08ac00";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "c38ed2c26ec4e242a0f4f280a520dfc3640e95101d301657c19a711ae0635d66";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "ff2d2bccfc4933081fb9ea663e0733b420eebcf37fb9ca22a5fa7fb57b08ac00";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "b35a74dc0149cc079ad2a92d14a10728a204ed5f353663f76e231360aff5a1f0";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "d6502d48613c078bdc6554d9bb07a305e94c86e327be402c511ee335d400e8d9";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "13ef1bcc6b9a8b16b87306f058c7af9676e94a3bcde25952510a81e13982a31a";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "fb2e7684b6c738b3d22575a8957bc08233112580d28f5474f37f684d894a4b4e";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "13ef1bcc6b9a8b16b87306f058c7af9676e94a3bcde25952510a81e13982a31a";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "fb2e7684b6c738b3d22575a8957bc08233112580d28f5474f37f684d894a4b4e";

// The CEK core's five staged-operand semantics steps, its staged failure hop,
// its writeBits loop and its BLS12-381 uncompress failure arm add eight
// manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-staged pin and
// the 576/571 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "87f793f758b34f79e1ca321299db78e7bed8687ff9a035690389970c9270fd7c";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "9bd16fff6efc0464d0095784d6ac85c5254cbc2fe90701c05b0303dfd060c814";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "4b39014e4f13c1e48fd5e79e98c30377ed203719c40feeacc5e473c7c7d71eed";
export const REAL_COMMAND_ORDER_DIGEST =
  "20845cf7f3f401eec495f1a78c31038b433d9ff67b1ef50f36f125036be4e8c0";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "4b39014e4f13c1e48fd5e79e98c30377ed203719c40feeacc5e473c7c7d71eed";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "20845cf7f3f401eec495f1a78c31038b433d9ff67b1ef50f36f125036be4e8c0";
