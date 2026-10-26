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
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "64bb85ef5a3d2eaf033b535cd0aeb626cd7768bc6a99b487bbd633c0dec0d9b3";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "1551824fd8101ebb81b5b018e1bfc7ada7e4fe94e902d93c48bf6af982d3af5e";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "c75f7144aa4c386736e9e08504efcd0ae134f422b5035aa5e5e72060fbcc6df0";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "1551824fd8101ebb81b5b018e1bfc7ada7e4fe94e902d93c48bf6af982d3af5e";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "c75f7144aa4c386736e9e08504efcd0ae134f422b5035aa5e5e72060fbcc6df0";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "707c77b87128735106ea630bbd3242751d86fd2a28a10c051232f3977cf61812";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "251e14f0abe99492bcebbdc5796f8d550c74267cd042154bfd74a168d63e7fa7";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "a518d73e6d06f6f23ed0f6cb1a93d4700ceb90c929b2d500f0818120a41f3f01";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "251e14f0abe99492bcebbdc5796f8d550c74267cd042154bfd74a168d63e7fa7";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "a518d73e6d06f6f23ed0f6cb1a93d4700ceb90c929b2d500f0818120a41f3f01";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "f05776fc733d092a1e48981448ad4855c8438ecb236d85f7017a59c808713e90";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "5aa8577bddde46691ff237d9d3b0c15301988e9b753c56ed2a909f70b47b3caa";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "6850a5594b02f389c45b46dc5338b386fb0afd0d52fd53c0eb448deaa17eace3";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "58498f740e787213cfbf336b3faf0ba16b550c625c79b0a98be0e910c6e7faed";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "6850a5594b02f389c45b46dc5338b386fb0afd0d52fd53c0eb448deaa17eace3";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "58498f740e787213cfbf336b3faf0ba16b550c625c79b0a98be0e910c6e7faed";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "0f440857ec201883d15f4ac7e03b6da05942d7c23240ac9759cb9464ef1235d8";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "9d3e0358d06278d3705b7f2f7607c101c05caa97ac66078fe98c9c64ea9669ba";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "84d33e5b0bb951627c554c8a488df3549b00705638d452567b23303105a2597e";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "b6cb718092115ad5918218cffaa64e2785669319943524788b01ddb117a66408";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "84d33e5b0bb951627c554c8a488df3549b00705638d452567b23303105a2597e";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "b6cb718092115ad5918218cffaa64e2785669319943524788b01ddb117a66408";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "6f87b37fda1a562a9634f1a1076217fa748f8bab041ebc9b2474ee1181b3f367";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "47687d97e84c001f10040e368401221cd2d55036d169d41834ce62d19e2daa4f";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "d595203d2d7e939739a7405144114c286d5d1e459e05208bcf160e11c3b2d796";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "d423181cfd33be205935fdabc12bec86557d50e8947336af7c53e09fd1b7f40f";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "d595203d2d7e939739a7405144114c286d5d1e459e05208bcf160e11c3b2d796";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "d423181cfd33be205935fdabc12bec86557d50e8947336af7c53e09fd1b7f40f";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "675e65c542ef371ca3a218a90d02cdacc8fa99677e22949c2ed187b3fbe53470";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "c12926e6c78f7e4702e5296162aa141da764bad39c0103cef264d51ff5cb3c09";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "d452b896366f93f66f999043d373ab93445fa0b123870713f90ed1b42261519d";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "f903714b4135c81a475f1acdbc2d817a49e766909d87ae36bee16bafa09ef86b";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "d452b896366f93f66f999043d373ab93445fa0b123870713f90ed1b42261519d";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "f903714b4135c81a475f1acdbc2d817a49e766909d87ae36bee16bafa09ef86b";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "f93df00d49f30c6399effa3a77a0c0e6d5450205c1b8476e4684e65777cc1bfb";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "839b6302e81f43693f69647c6e905d8402995e239e1b66c23f68299acdbf0729";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "c66082c1917a6891f02eb1955e4dd3b496e6ebf076a74043c3788aaa6df3e698";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "8285a2b74d165cd226e51a94df01637e199a311a5b6e4efd39ce1a938b3f2bb6";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "c66082c1917a6891f02eb1955e4dd3b496e6ebf076a74043c3788aaa6df3e698";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "8285a2b74d165cd226e51a94df01637e199a311a5b6e4efd39ce1a938b3f2bb6";

// The CEK core's five staged-operand semantics steps, its staged failure hop
// and its writeBits loop add seven manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-staged pin and
// the 576/571 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "4cd7b42edfb01154c99e94e03c4374c92592632688c9aa0bc754b6ec5c07ba17";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "5cfd8a84504ca55851649cabda303c30ab5f6a1c3f1ee1c9909581f353ddd395";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "6c4ce1f6d5de002f1321f70076dc1dfb4ba2f557d9578ec196a8bf74998174e5";
export const REAL_COMMAND_ORDER_DIGEST =
  "cd18b41f4b38b678fb6ff27195c452a8cf6c01962e011e50777c5faf9aaf77cf";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "6c4ce1f6d5de002f1321f70076dc1dfb4ba2f557d9578ec196a8bf74998174e5";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "cd18b41f4b38b678fb6ff27195c452a8cf6c01962e011e50777c5faf9aaf77cf";
