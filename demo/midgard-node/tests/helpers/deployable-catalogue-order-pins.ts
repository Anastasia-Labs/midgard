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
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "5a7500fc66d04da4c575ee9e5346069dab1340508a4997961fc183928cc231b0";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "7424cbbf668217a143aa5fe4af23bfcd012474e60bb3f512aa620856b98d71bc";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "a3cfe1be86dca45a2d26fa84e3f5358d81e9b1d272253f49e41927a4624c53a7";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "7424cbbf668217a143aa5fe4af23bfcd012474e60bb3f512aa620856b98d71bc";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "a3cfe1be86dca45a2d26fa84e3f5358d81e9b1d272253f49e41927a4624c53a7";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "0c2de24d352525d83afa403900d34e7b61d72266e2abbb8064ebe1fd4d99180b";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "32d35213f7ff0193be0d46423f195d9a04846bdb45cd9938b934f403c4b83153";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "8ed0a35d7824d11b48d6180cbb7840f1207f00ccc4922d1336b1a7c4a08ce534";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "32d35213f7ff0193be0d46423f195d9a04846bdb45cd9938b934f403c4b83153";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "8ed0a35d7824d11b48d6180cbb7840f1207f00ccc4922d1336b1a7c4a08ce534";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "b3ac1f2db9be5078e75e3ff73df3efdf78ac6d9b86a4c6f08699dae3a3734882";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "d60a454171e94f64676183519002f061a72c2bb462f85f8dcfa65d63475e0c2b";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "3cc68853caa422580614de3e90f1bfb88af79adc360f00df3e407384e219bfe7";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "94ee06f1f7aa7e3eea6d9b5134eeb16dcb19d36c6103d759f1a574aae1a6844b";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "3cc68853caa422580614de3e90f1bfb88af79adc360f00df3e407384e219bfe7";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "94ee06f1f7aa7e3eea6d9b5134eeb16dcb19d36c6103d759f1a574aae1a6844b";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "a3277236d3532f9e6d0ed5af6411b6b9a68ef6af8a260fcbb0095277bbb4135a";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "7e494202f1ad4e8ab86cc1e073ca08df4f17e8ad3ac7c8d8034a8850747659a3";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "f9e77e6b1a3515debe7ad6f783270325c66609a85b23a535226557c9e7589e2b";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "8ba05f26c7f636fb41fdec7c22a745b222dd34a095eb0a63a3e8d87e214fe1e2";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "f9e77e6b1a3515debe7ad6f783270325c66609a85b23a535226557c9e7589e2b";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "8ba05f26c7f636fb41fdec7c22a745b222dd34a095eb0a63a3e8d87e214fe1e2";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const PRE_CEK_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "4b59045c43835fdf066e7df27e25cafc9eccbbfb50fa7587f2ed4addddf18a91";
export const PRE_CEK_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "f7a24e50518de1065ed177f2fe3026b37641838e3a0b085531110b4ff9653177";
export const PRE_CEK_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "1603e45aae1d2f5a52e751236a2db8dff0e089ed7152c09595ebd057589c7a17";
export const PRE_CEK_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "81f2298d1124412c25cf1d9cb446379f820cca11b8c117c948c89eb0f7ea73a2";
export const PRE_CEK_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "1603e45aae1d2f5a52e751236a2db8dff0e089ed7152c09595ebd057589c7a17";
export const PRE_CEK_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "81f2298d1124412c25cf1d9cb446379f820cca11b8c117c948c89eb0f7ea73a2";

// The CEK phase's one-step resolver and its four semantic resolvers added
// five manifest entries, each a published role. Projecting out exactly those
// entries reproduces every pre-CEK-resolver pin and the 523/518 counts.
export const PRE_PHASE_RESOLVER_MANIFEST_CONTRACT_ORDER_DIGEST =
  "94f7110b779897ffbd9d1765f573de78cf264f598cc28ecba994c0507dd1c40c";
export const PRE_PHASE_RESOLVER_MANIFEST_ROLE_ORDER_DIGEST =
  "72184dc9b5bdf8bc9a25d40260f40cccc33211ced87fb4eee37129a8203db4a1";
export const PRE_PHASE_RESOLVER_REAL_PUBLICATION_ORDER_DIGEST =
  "d317bd73b92b3a5be369a035e8ee00c29b7403669c5d8a682199bb8cc281f233";
export const PRE_PHASE_RESOLVER_REAL_COMMAND_ORDER_DIGEST =
  "b6e13ea622b37a17a0354fbb243ef86bfbba89c90ff31f295b0f652b327d1393";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "d317bd73b92b3a5be369a035e8ee00c29b7403669c5d8a682199bb8cc281f233";
export const PRE_PHASE_RESOLVER_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "b6e13ea622b37a17a0354fbb243ef86bfbba89c90ff31f295b0f652b327d1393";

// The validation phases' one-step resolvers and their semantic resolvers add
// manifest entries phase by phase, each a published role. Projecting out
// exactly those entries reproduces every pre-phase-resolver pin and the
// 528/523 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "d92c2ef70ca093644a8993a65f40a58d18b7970e730b6f19d01fbfb973d11c6b";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "1f2f62609d4497c2de011d980f707e83fd7b09d5c99482b5b1f3d8f7ecc99c8c";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "0ba9dc319b41f650b162c17b9b6d015c8423effdcca5fbee2f41989ea6e68c6a";
export const REAL_COMMAND_ORDER_DIGEST =
  "df51afa18ea7a9768fc7916e9a9da8428a184d5a1c18a6446a597ab41a10e850";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "0ba9dc319b41f650b162c17b9b6d015c8423effdcca5fbee2f41989ea6e68c6a";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "df51afa18ea7a9768fc7916e9a9da8428a184d5a1c18a6446a597ab41a10e850";
