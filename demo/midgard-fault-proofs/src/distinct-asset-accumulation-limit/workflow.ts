/** The `distinctAssetAccumulationLimit` family's direct walk. */
import { defineDirectWalkFamily } from "../direct-purpose-family/walk-family.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY } from "./contracts.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_FIELDS } from "./custody.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_ARTIFACT_FAMILY } from "./replay.js";

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_DIRECT_WALK =
  defineDirectWalkFamily({
    spec: DISTINCT_ASSET_ACCUMULATION_LIMIT_CURSOR_SPEC,
    family: DISTINCT_ASSET_ACCUMULATION_LIMIT_FAMILY,
    artifact: DISTINCT_ASSET_ACCUMULATION_LIMIT_ARTIFACT_FAMILY,
    replay: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
    fields: DISTINCT_ASSET_ACCUMULATION_LIMIT_FIELDS,
  });
