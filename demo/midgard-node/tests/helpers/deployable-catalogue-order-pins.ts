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
// command) reproduced every previous digest of every generation.
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "39109fc22dc15fe2583fc34ee93cc1425b24ad0e2792e566f5c43954972cd9c2";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "d679c1ee58ca7e6934d85ebf806a1f19f5f0dd50cb9b86b461a8f9422d82b1e2";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "4812036bf22d95124711c52db02ba68d6104760f3b277ad80201aaddb9483b11";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "8f1968e36b1b8b3f70d9a9b2ed267ce401f28ec8e841f71678f5ac8b349e1685";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "6321df60647f510c6c11e671cb82fc56183d76a0107c8e5759d6f45a46a2dd05";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "2b95e3a2e9422f9eb1bacf2430ef9c1fae88a78b52ee6d4eacfb86851347c225";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "6f980ec4e56ea776b65f7e3b7b51ecb694ca50e47f1354dc3f589b81025c29c4";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "ed8636a35b33b0886ac1caf6413ab4112e2b8ebbb3c9d0357c257284ab5fbf06";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "47bc8ac576a5103c498b0085ac48f9ba32944d782b13cc20a09449d4220bfa21";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "49fc66395db48d6337c0e13a45e11dacba7e60ce7204ba6d54b05301445abd95";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "33d96c0a774c3b21188c2381b3def67d60ad2d8a89659fe2cc4d961351070967";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "65a9a07a55d8b124b30d8080d8288d472bde0020bfb3ff872da3cea1e5176966";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "61d40b971a4405f28ef3881150ceb47d3f72fd8288df19aa54483c4f3ff717a6";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "16fbd7f46df46aedb0a7dbe841943104c84af7c06f73f3719846e6e4608ea8ab";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "488c045ab15ac021873a87c8809ead3cf71b5f8a910e986c01cbbbfcad677150";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "993c9f270b936ead6e61caec004a371abaeeff1258c20fa66e2dfa484f04a7af";

// Projecting out all four data-canonicity additions leaves 514/509 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "fb7a7b663af3595cfe5a1974a258ac64cee7b8982d6c71d852fbd0a42beb48db";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "b36abb652d83bf0d7de86beaa0d4c9ecfb746a0744b160afa7b37654683c4fa4";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "642e00d786931c6a649fabd7006200f25d2c4624767851f32a2d7bc451d264b7";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "cd69cedaffccb21675137713a33b9f5e59825f56648096a1cc5a0b5c62132a96";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "ab6e926167032218cc8e7ddc37d093100f04d90865c2a17a72a7d133fe20dc71";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "1ca62077560185e240202ee0bb3360d24447a310190fb9f388b4fe01722db31a";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 518/513 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "c46b04d588492c87b01f79b48ff00cf069562c179de1bf0720fe507113bbabda";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "1a01e7358b090f70b516dfe09bb87be596d3b6ba61391448666e26225bfed6fe";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "4e457df7f0543b1510e48da6dc8cef542171f3c09438d32e109512d45db67d8c";
export const REAL_COMMAND_ORDER_DIGEST =
  "3484562a4d25e72f6dcdd58774b2d9b74524e0d7648a8c5277bc5b0418d605a4";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "1a9ef1b663279943a722a91fe4d66ec619566631bdd22c112f38acea7875524d";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "dd66a6862fbd114c17c4ae294eb139bb8957e5d64d9a81c5852af3735a888362";
