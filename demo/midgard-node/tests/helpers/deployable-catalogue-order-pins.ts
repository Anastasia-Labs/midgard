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
export const PRE_HISTORY_MANIFEST_ROLE_ORDER_DIGEST =
  "c68fb6aace2567420095fced89151672b2206e10435ed16c65e8838170262f51";
export const PRE_HISTORY_REAL_PUBLICATION_ORDER_DIGEST =
  "eef64fd401b648b79f4219d6c7e9c0df78be513a0f045c5a65a9e4fb409e214a";
export const PRE_HISTORY_REAL_COMMAND_ORDER_DIGEST =
  "c39117d840cbd3a7cc1d953cc6ad8048df698b8d829cf136a81583f2215dfc4d";
export const PRE_HISTORY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "ff9281ce5a3d8877fcf777994a6f65cc19ac38a247bf6de458cd5b2adc1f2521";
export const PRE_HISTORY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "07122773835fd062fc5ef6544f497865c16425442d3c13ec9e9ceca40e7e6ca6";

// The history pins were derived only after projecting out exactly these six
// entries reproduced every original order digest and the original 528/521
// counts; they are in turn the orders before the two DA bond pool entries.
// Both older generations still listed the per-header bond yield the pooled
// bond retired, so each projection restores that one role (see
// `withRetiredBondRole`). The retired contract's name is not restored, so
// the older contract-name pins are dropped: the older role pins still fix
// every published entry, and the current contract-name pin fixes the rest.
export const PRE_POOL_MANIFEST_ROLE_ORDER_DIGEST =
  "d3cb72a65020d36c60c76be7589f9e3b68baf99892c4ae8f2e02b8272f9e5343";
export const PRE_POOL_REAL_PUBLICATION_ORDER_DIGEST =
  "0670accf7467bd79d8bd1588066d45fa2081de36384d891b529f80b15e93955e";
export const PRE_POOL_REAL_COMMAND_ORDER_DIGEST =
  "eaf82736cf5f9ec5495b5e0e6a1512192c3558adfae05662cd4adad1c84f4ab5";
export const PRE_POOL_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "eef250aa0610302c1afb12201769fb6a0083c355bff80f401843aeaf06aac3f3";
export const PRE_POOL_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "3cc8bac05e38516f6ef5292b2c8d97dfb0e4a0e8c2d9a6553b461f7ee283efd8";

// New pins are derived only after projecting out exactly the two pool entries
// (with the retired bond yield restored) reproduces every pre-pool published
// order digest and the pre-pool 534/527 counts.
export const PRE_DATA_CANONICITY_MANIFEST_CONTRACT_ORDER_DIGEST =
  "8be714ebe04c5b70477715280435c4df87b93872cffb073867a6740b69ac5c1f";
export const PRE_DATA_CANONICITY_MANIFEST_ROLE_ORDER_DIGEST =
  "81b8e4d09c0d25e1a024cd3b28240dfaf6188ca4d49f0b93369341b7dfcf67c8";
export const PRE_DATA_CANONICITY_REAL_PUBLICATION_ORDER_DIGEST =
  "cdadb143599388810488d20c274cfb31e1f85f8526bfe4a6dfc62e033a0add91";
export const PRE_DATA_CANONICITY_REAL_COMMAND_ORDER_DIGEST =
  "a44d27371ef984600d8c78c5d1fc0eb1037b133c906da91293dbd0bbff5f55e2";
export const PRE_DATA_CANONICITY_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "ddee6dae4ef81c8b64091a531745e1dc0fc14fb88d97db91dedd36351430c51c";
export const PRE_DATA_CANONICITY_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "4aeac0be36e07f0a6aa9b9a38526aac2440a712981ee0da5b575b1a5ee3e473f";

// Projecting out all four data-canonicity additions leaves 511/506 entries.
// The retired escape-hatch manifest fields had two null roles; restoring them
// reconstructs every earlier order pin without moving surviving entries.
export const PRE_CASE_MANIFEST_CONTRACT_ORDER_DIGEST =
  "96d5746ccd18de9194f4c72cdb37dbc63f616298624ccf2095a624f76639267a";
export const PRE_CASE_MANIFEST_ROLE_ORDER_DIGEST =
  "ab98acc6f11bd0624b7da66732a409a2dd27ed09d5066317ca9dbf6c0acde969";
export const PRE_CASE_REAL_PUBLICATION_ORDER_DIGEST =
  "33bd457dc0ef00b5b1cd51a522f96a1eb11e429e047efcc14b61d3fdef879174";
export const PRE_CASE_REAL_COMMAND_ORDER_DIGEST =
  "ee0a86066bdc3d75891594e0dccdc8be69d263796a9da511eeb8d0a6882b27e2";
export const PRE_CASE_PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "d0122b56094f45603ab0ef5850f6a8dd08eccf26d1ef2d4fd8a8cbd0f0b6a344";
export const PRE_CASE_PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "9e2e5454b0bbdc8b93c3f6e4eac3f09be41dd9fb20caecb8fe2d5334401a250a";

// The validation case added six manifest entries, each a published role.
// Projecting out exactly those entries reproduces every pre-case pin and the
// pre-case 515/510 counts.
export const MANIFEST_CONTRACT_ORDER_DIGEST =
  "57b9907011c202fb89b6c652c75f360d98417ee2b1d9a4d4fcfe330b910d1dd3";
export const MANIFEST_ROLE_ORDER_DIGEST =
  "c200cf1bf35b5822457661deea8b4c231154d4f67dc9bee2c89d6b68feec6d62";
export const REAL_PUBLICATION_ORDER_DIGEST =
  "b5c8b106b50fffa0b6d6d515928b550af84deaa61c690149a2f59e54bb44c5a2";
export const REAL_COMMAND_ORDER_DIGEST =
  "2f8202833faf562fdca6dc94bfae2ed9a6d757e3fec05b37407d2c71d8bd9869";
export const PLACEHOLDER_PUBLICATION_ORDER_DIGEST =
  "cb206f89b4972178ddb4ce5134d0d0da7035fdfb95ca84afaf078bc715823022";
export const PLACEHOLDER_COMMAND_ORDER_DIGEST =
  "607cd716a7168cacfd7df7f246c22d5b9fdfd4a07b31b2c13bd82d23c7bd7b85";
