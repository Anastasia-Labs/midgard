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
// They were re-derived the same way when the missing-redeemer family grew
// from its seven step contracts and roles to the 39 of the direct-derivation
// graph. Replacing its contiguous block with the previous seven entries
// reproduced every previous digest and count of every generation.
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "2576fbaa067754c7da82b9acc411e1ad1364966967d1828750e837427430b57d";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "0138224132a92d7cfe79177e7ab1779665c3b5742289cb88fb72b447c3ae0eb5";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "5d1377354753c3e0b59fd558b0b5d3e1a7cfc5d94066908059a7675e8e37cd6c";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "0138224132a92d7cfe79177e7ab1779665c3b5742289cb88fb72b447c3ae0eb5";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "5d1377354753c3e0b59fd558b0b5d3e1a7cfc5d94066908059a7675e8e37cd6c";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "bd4f4d09dc294538c3d74a697e98e85b8eec353cf2398ee2e8d316cc383c1b48";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "d5720298febd4fae99cd7f404d97e3dea3fa15a81ba561d2b3ba347db7f9ee7b";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "5e02c13b156d147a4745a7a0fc1799fc214107858f6fe1ea56f7da359d74930a";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "d5720298febd4fae99cd7f404d97e3dea3fa15a81ba561d2b3ba347db7f9ee7b";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "5e02c13b156d147a4745a7a0fc1799fc214107858f6fe1ea56f7da359d74930a";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "b12e4cd9a3b5eef182e5e67de2305c635f6bdab3d50693ed2cc0227bce989315";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "bd2676140aeb364c20ba846e13b28be0aee19d79fd81bd6f640ffb951dd34d42";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "f407092adb189f71e3ecd95a28fca305ce0f2f206a93963ba2df01768c10024e";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "c7d6d22aa917a762ca252743a593daab82b63198faed342028458dc0c89eae3d";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "f407092adb189f71e3ecd95a28fca305ce0f2f206a93963ba2df01768c10024e";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "c7d6d22aa917a762ca252743a593daab82b63198faed342028458dc0c89eae3d";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "bc9061bd6adb27451149311e73f03ea89528f0807045b8e928bf2e96091dd281";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "013775d48915b329d1e491cf5a3db18ee4b78b5b5b52088e29db93b0be96ee9c";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "0b02e4b2a6dde6ee82df70d55a651d6e3818c6fcbcb5409868984fba90b16e96";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "bc2d7c2540cdfab477cce7b7bd35952d171a682797c657ddae3ee1d50d5d300d";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "0b02e4b2a6dde6ee82df70d55a651d6e3818c6fcbcb5409868984fba90b16e96";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "bc2d7c2540cdfab477cce7b7bd35952d171a682797c657ddae3ee1d50d5d300d";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "e5109647a0364875e4e61be043929c35bac036bcdac61bc38a12dc78dce7ba58";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "8648a7d6ed042282e01cfbff2e501801b0f8c00fc356bddcda1d16f6a497b054";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "29967d5b14d58419b3841cbf880246c8dda142399ebee8b251854beb825e084f";
export const REAL_COMMAND_ORDER_DIGEST =
  "b2a67da47591570480f46af98ae0ae24a84b203f4c7878032d582cd3a9ac4668";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "29967d5b14d58419b3841cbf880246c8dda142399ebee8b251854beb825e084f";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "b2a67da47591570480f46af98ae0ae24a84b203f4c7878032d582cd3a9ac4668";
