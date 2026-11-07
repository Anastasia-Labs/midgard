/** The script-sources redeemer item chain's own holders, each the script
 * of the deployment manifest member its contract name points to: the
 * invalid header, tail, data and scalar data executors and the settlement
 * the runner pays a thread into hold the very scripts the manifest
 * publishes under their names. */
import * as SDK from "@al-ft/midgard-sdk";
import { beforeAll, describe, expect, it } from "vitest";

import {
  SCRIPT_SOURCES_REDEEMER_CONTRACT_NAMES,
  scriptSourcesRedeemerValidators,
} from "../src/validation-trace-dispute/one-step-script-sources-redeemer.js";
import {
  type Scene,
  setUpScene,
} from "./support/validation-trace-dispute-open-scene.js";

describe("validation-trace one-step: the script-sources redeemer item chain's own holders", () => {
  let scene: Scene;
  beforeAll(async () => {
    scene = await setUpScene();
  }, 300_000);

  it("holds at each of its own holders the script of the manifest member its contract name points to", () => {
    const validators = scriptSourcesRedeemerValidators(
      scene.chain.scriptSourcesStageOneRedeemerStages,
    );
    const pairs = Object.entries(SCRIPT_SOURCES_REDEEMER_CONTRACT_NAMES).map(
      ([holder, contract]) => [
        validators[
          holder as keyof typeof SCRIPT_SOURCES_REDEEMER_CONTRACT_NAMES
        ].spendingScriptHash,
        SDK.DEPLOYMENT_RECIPES[contract].resolve(scene.h.contracts, {
          phasMembership: () => {
            throw new Error("a spending member reads no membership script");
          },
        }).scriptHash,
      ],
    );
    expect(pairs.map(([held]) => held)).toStrictEqual(
      pairs.map(([, published]) => published),
    );
    // The holders are distinct scripts, so a swap between two of them shows.
    expect(new Set(pairs.map(([held]) => held)).size).toBe(pairs.length);
  });
});
