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
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "409bbb7270aa4f33121805c01a60a5505070d59786d2046aef761b15cad58dab";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "c3e7f10475123d00dc205ae8e16d5da57b8cf441fdb59a3f85d1ebc7938bdbf8";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "1b23cde6583c8b6c28a12ea4bf64d0f44cbdddc3f784997bd8a6659ab8ee8d07";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "c3e7f10475123d00dc205ae8e16d5da57b8cf441fdb59a3f85d1ebc7938bdbf8";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "1b23cde6583c8b6c28a12ea4bf64d0f44cbdddc3f784997bd8a6659ab8ee8d07";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "ff658e9c213fa8b40ebe82fb6b4d2a45e2daac480af165d867fd7927e568ad36";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "220879caace44fcd305412f314d780e77f8fab989d68f5d388f9f36611013f8c";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "a02bd0ef36b3f01691f66f71e659996189d644365668fb3c173d8f323ff617b7";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "220879caace44fcd305412f314d780e77f8fab989d68f5d388f9f36611013f8c";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "a02bd0ef36b3f01691f66f71e659996189d644365668fb3c173d8f323ff617b7";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "beb89f540417764305c9bccdb7ef8a75dbc37c63b57957183a73c7d94b8e686a";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "34b0b53553fa5fb65f681ce6dc1121eb5a794ed6a0bfd46720518b095430dd55";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "0205441e4729a74758851f86ef10b67f08ed8b65dfcd1885f86eedbdc17a27f3";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "e846a3ad21cd7e3a9b413a98b8f75ff0b490970b726a2686487b17a5b0886413";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "0205441e4729a74758851f86ef10b67f08ed8b65dfcd1885f86eedbdc17a27f3";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "e846a3ad21cd7e3a9b413a98b8f75ff0b490970b726a2686487b17a5b0886413";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "6eff262da52dbe3c44721e0b25fcea298b8aaa87a57fc4e30fd6f0da0e6cfb87";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "70b5a97e50fedc8aa4ab1ba837036a6cb1ce19bfbb634e23571ccbd164c11191";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "071c0905eb7d5fc635a0971556a10cc74dcb3944625b7dc9343fc30e31f363bc";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "1d497fa215132221ed7355f72d6e44db29c81140305bc21298321c855b483edf";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "071c0905eb7d5fc635a0971556a10cc74dcb3944625b7dc9343fc30e31f363bc";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "1d497fa215132221ed7355f72d6e44db29c81140305bc21298321c855b483edf";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "4d09fb2ac81ebd9a743b09bae6706749ba754a80019447169bb659385021a9a3";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "b1b8b9ca7bdc82c2e13d11d23b6c42743e21c4f69cad841613d9211dfcf653cb";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "b80f0daa83b17886edcda9b6fd2b2eea8f67659d42146232ef6ff4216b9cf82c";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "ad0222984b04ef8e31789831ebff7d2aa6a6a44c0e84387805bef7a5ef7cbe12";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "b80f0daa83b17886edcda9b6fd2b2eea8f67659d42146232ef6ff4216b9cf82c";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "ad0222984b04ef8e31789831ebff7d2aa6a6a44c0e84387805bef7a5ef7cbe12";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "f124a6a21713f95c60826590da6bad6c220c85e3e02500ff5b89434ee663cf9b";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "853d0bac5783af2b0730029e2e29c5f83c37eaebc96472ddaeddfc051173044e";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "2a4e1ffa201ebd2151e4ff35467a8ce3d2a14a6963913306f1ac85b0891b5fa3";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "0098ff3e6a71bddfb05ccf9781b5a1355ccc1e622bef625673ace4b217841f1f";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "2a4e1ffa201ebd2151e4ff35467a8ce3d2a14a6963913306f1ac85b0891b5fa3";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "0098ff3e6a71bddfb05ccf9781b5a1355ccc1e622bef625673ace4b217841f1f";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const PRE_CEK_STAGED_MANIFEST_CONTRACT_ORDER_DIGEST =
  "9a886fd6168266e456b567b86cd86b9ff38728d67ee7ac5e4933115664ae24b3";
export const PRE_CEK_STAGED_MANIFEST_ROLE_ORDER_DIGEST =
  "dd7cd4ca12dab3e9e6c2102d8078dc32d9057524606bcd93babcdf56af9762ea";
export const PRE_CEK_STAGED_REAL_PUBLICATION_ORDER_DIGEST =
  "10eed43bb0a0b00123fbad0c0428e9545bd87162597470d7d7d10ea2154b7cb0";
export const PRE_CEK_STAGED_REAL_COMMAND_ORDER_DIGEST =
  "eb7f41f0b393f3379b4d38c9cb96d5d41251d27c5cc8a912b2eb970001c359f2";
export const PRE_CEK_STAGED_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "10eed43bb0a0b00123fbad0c0428e9545bd87162597470d7d7d10ea2154b7cb0";
export const PRE_CEK_STAGED_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "eb7f41f0b393f3379b4d38c9cb96d5d41251d27c5cc8a912b2eb970001c359f2";

// The CEK core's four staged-operand semantics steps add four manifest
// entries, each a published role. Projecting out exactly those entries reproduces
// every pre-staged pin and the 544/539 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "6a11dcf1f2164653af050a0fa87fa2cc5d8e128e48a0e03c4d7872f19e458a1c";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "321b2e89785f3bd83a05616cb301b0ce2506b3dd5ccf8f6948424c6fa71457cb";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "85aea426b08d96f5c355634c7d447eb62ca3a1cdece31e41a25505eabad76cac";
export const REAL_COMMAND_ORDER_DIGEST =
  "c581d1b8f3b2c7f6598c2fc21a10ff7035bab69e5721b40c6068351e18b61ad3";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "85aea426b08d96f5c355634c7d447eb62ca3a1cdece31e41a25505eabad76cac";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "c581d1b8f3b2c7f6598c2fc21a10ff7035bab69e5721b40c6068351e18b61ad3";
