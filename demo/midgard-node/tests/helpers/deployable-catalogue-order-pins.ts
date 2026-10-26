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
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "2233ff1a7972bc426a4ca83a1f19960389c8275713a1007e4cf9bd27c11751a4";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "949a99e259715ca6ec7e84d1a31ac5c933da447389303c0e4c7cf034aa000cd6";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "e043b8522a83e9ee6a82ff88e24eff35e2fe7e9ee72b1180f0fdcd33e46d1f9e";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "949a99e259715ca6ec7e84d1a31ac5c933da447389303c0e4c7cf034aa000cd6";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "e043b8522a83e9ee6a82ff88e24eff35e2fe7e9ee72b1180f0fdcd33e46d1f9e";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "7f1b086c1ba7eb1279bdc5910841f14da04f29e01518c4ecf470fc4518a9cbd0";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "b98314e9078d2a62411d5147a98b9f773c044a28442164ed5fd5b6c874271491";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "3120d7447c59346c7b1d597df80c1acccf1e0b944495936a510f4cca4889abea";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "b98314e9078d2a62411d5147a98b9f773c044a28442164ed5fd5b6c874271491";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "3120d7447c59346c7b1d597df80c1acccf1e0b944495936a510f4cca4889abea";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "2260b40cf3d27f3da79dd446b041e28d871e8b89077c764743930f92ba81e6a7";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "fbcfbe6b1f35ca708e386beededd529f82c8aaf152279ac78a19b7cdde17b59c";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "6a5f20dd56141b1e1a86214d58a99e00877f1bc22bbcf388462f427998257b92";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "8c4d831fceabf6597ec92f53aa600fe2b4f55f20152a399ce1bb273e71441a1e";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "6a5f20dd56141b1e1a86214d58a99e00877f1bc22bbcf388462f427998257b92";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "8c4d831fceabf6597ec92f53aa600fe2b4f55f20152a399ce1bb273e71441a1e";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "ebe0845a626c8315a026820da867d1c8bfce35b4e47d1423e041dc6f03774e4b";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "1f1d0af348eacfd3ec10663f5c2743255ef62f81edb8bd63fab78d144262eb89";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "73fcb205ab5cbb7a776182f4af5cd036de351faedd9093555ce40dac7b224c44";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "75ccfd6d49539ec9cafbb0004256f18f8b7fb1a4ae58f8eab23291ce8958d214";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "73fcb205ab5cbb7a776182f4af5cd036de351faedd9093555ce40dac7b224c44";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "75ccfd6d49539ec9cafbb0004256f18f8b7fb1a4ae58f8eab23291ce8958d214";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "41010def08e52a2359878bde7189a266eb7807fb264d03e3ee450a8092cdfecf";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "dfb17b86dd7ff88288c4741b491aa6a36b40154133c31ab78bdf97d37570a1f0";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "79309516da6eb0543582dcb25344922033cbc096faea9a73c9a1f2476bec80fe";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "6e5729586c7f934477342bbf5b0d58d0cb898269489e14668f52b1d8c4e33534";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "79309516da6eb0543582dcb25344922033cbc096faea9a73c9a1f2476bec80fe";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "6e5729586c7f934477342bbf5b0d58d0cb898269489e14668f52b1d8c4e33534";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "6580d5ca523f205add260e77b82f5e27b96820119224703c5c2ad2509e2a6a34";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "df582e7139c583cc8313b9967161789e7394d9b18ca346f3cf9be3d4215c160a";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "f2296456b0ee59a707f7a660770e0470e9f5d899c049e7fb0570b8b860749bca";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "c99da4717598778015c1ce568c8a37d5829c76947fa022d21e08312ae8b99b7d";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "f2296456b0ee59a707f7a660770e0470e9f5d899c049e7fb0570b8b860749bca";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "c99da4717598778015c1ce568c8a37d5829c76947fa022d21e08312ae8b99b7d";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "7d132a59ac64e65e5a56bf04ffbbeadc60e1d75a1432e682faaad349426a6ccf";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "aaddf7554cbbe53b2b60cdd57e3f3a76d1f0c3b93320526a5a8ae7db212c1ed4";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "14bc0d6a5c4384dd9f0f5e29d3aa84bcf627fb979ae5bfc825a26550415caac1";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "792da8e5d9888765f799f6502478705fb7e2f35aeb65e6e1207ba9b03ff25fe9";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "14bc0d6a5c4384dd9f0f5e29d3aa84bcf627fb979ae5bfc825a26550415caac1";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "792da8e5d9888765f799f6502478705fb7e2f35aeb65e6e1207ba9b03ff25fe9";

// The CEK core's five staged-operand semantics steps, its staged failure hop
// and its writeBits loop add seven manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-staged pin and
// the 576/571 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "51ca0e3c802ee01ba4bdbfc0a937afb0496bd9d122e08fcde6adc20ffca763a2";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "48c57c5d38b49bf0c39edcd19fca9ef8e0dfe0d867ba4905839971070d190a9b";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "91c420fc03ac180e25a76e19446f75d01d0afc81a1384992cf5a5ee6e7da4bde";
export const REAL_COMMAND_ORDER_DIGEST =
  "3fe328b78fff5f7ea3b970b1f82ca680c2734b2ec0979ccdbb16254e417b96a4";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "91c420fc03ac180e25a76e19446f75d01d0afc81a1384992cf5a5ee6e7da4bde";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "3fe328b78fff5f7ea3b970b1f82ca680c2734b2ec0979ccdbb16254e417b96a4";
