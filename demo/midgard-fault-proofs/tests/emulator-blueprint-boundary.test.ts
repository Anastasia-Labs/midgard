import {
  makeSpendingValidator as makeSdkSpendingValidator,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import {
  applyDoubleCborEncoding,
  applyParamsToScript,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  alwaysSucceedsBlueprintPath,
  applyCompiledScript,
  cloneBlueprint,
  getCompiledScript,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";
import {
  makeAlwaysSucceedsContracts,
  makeAuthenticatedValidator,
  makeIsolatedAlwaysSucceedsAuthenticatedValidator,
  makeMintingValidator,
  makeSpendingValidator,
  makeWithdrawalValidator,
} from "./support/emulator/validators.js";

const title = "computation_thread.mint.mint";
const policyId = "ab".repeat(28);

describe("emulator blueprint application boundary", () => {
  it("preserves raw schema metadata through loading and cloning, and rejects wrong shapes", () => {
    const blueprint = cloneBlueprint(readBlueprint(realBlueprintPath));
    const normalized = parseFaultProofBlueprint(blueprint);
    expect(
      normalized.validators.find((entry) => entry.title === title)?.parameters,
    ).toEqual([
      {
        title: "fraud_proof_catalogue_script_hash",
        schemaRef: "#/definitions/ByteArray",
      },
      {
        title: "hub_oracle_script_hash",
        schemaRef: "#/definitions/cardano~1assets~1PolicyId",
      },
    ]);
    expect(() =>
      applyCompiledScript(blueprint, title, [policyId, "ab"]),
    ).toThrow(/28-byte hash/);
  });

  it("preserves applied bytes and refuses missing, extra, or bare parameters", () => {
    const blueprint = readBlueprint(realBlueprintPath);
    const entry = blueprint.validators.find(
      (validator) => validator.title === title,
    )!;
    const params = [policyId, policyId];
    const expected = applyParamsToScript(entry.compiledCode, params);
    expect(applyCompiledScript(blueprint, title, params)).toBe(expected);
    expect(applyCompiledScript(blueprint, title, params)).toBe(expected);
    expect(() => applyCompiledScript(blueprint, title, [policyId])).toThrow(
      /declares 2 parameter/,
    );
    expect(() =>
      applyCompiledScript(blueprint, title, [...params, policyId]),
    ).toThrow(/declares 2 parameter/);
    expect(() => getCompiledScript(blueprint, title)).toThrow(
      /declares 2 parameter/,
    );
    expect(() =>
      applyCompiledScript(blueprint, "missing.validator", []),
    ).toThrow(/not found/);
  });

  it("rechecks schema and arity even when the exact code and values are cached", () => {
    const original = readBlueprint(realBlueprintPath).validators.find(
      (entry) => entry.title === title,
    )!;
    const parameters = original.parameters!.map((parameter) => ({
      ...parameter,
    }));
    const blueprint = { validators: [{ ...original, parameters }] };
    applyCompiledScript(blueprint, title, [policyId, policyId]);
    parameters[1] = {
      ...parameters[1]!,
      schema: { $ref: "#/definitions/Int" },
    };
    expect(() =>
      applyCompiledScript(blueprint, title, [policyId, policyId]),
    ).toThrow(/must be an integer/);
    parameters.pop();
    expect(() =>
      applyCompiledScript(blueprint, title, [policyId, policyId]),
    ).toThrow(/declares 1 parameter/);
  });
});

// Captured independently with Lucid's direct identity functions.
const script = "46450101002499";
const hash = "186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4";
const preprodAddress =
  "addr_test1wqvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqxy2mj0";
const mainnetAddress =
  "addr1wyvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dqav78a2";

describe("emulator validator identities and explicit scaffolds", () => {
  it("preserves all four constructor identities and the harness network", () => {
    const spending = {
      spendingScriptCBOR: script,
      spendingScript: { type: "PlutusV3", script },
      spendingScriptHash: hash,
      spendingScriptAddress: preprodAddress,
    };
    const minting = {
      mintingScriptCBOR: script,
      mintingScript: { type: "PlutusV3", script },
      policyId: hash,
    };
    expect(makeMintingValidator(script)).toEqual(minting);
    expect(makeSpendingValidator(script)).toEqual(spending);
    expect(makeWithdrawalValidator(script)).toEqual({
      withdrawalScriptCBOR: script,
      withdrawalScript: { type: "PlutusV3", script },
      withdrawalScriptHash: hash,
    });
    const mintScript = "525101010023259800a518a4d136564004ae69";
    expect(makeAuthenticatedValidator(mintScript, script)).toEqual({
      ...spending,
      mintingScriptCBOR: mintScript,
      mintingScript: { type: "PlutusV3", script: mintScript },
      policyId: "bd3ae991b5aafccafe5ca70758bd36a9b2f872f57f6d3a1ffa0eb777",
    });
    expect(makeSdkSpendingValidator("Mainnet", script)).toEqual({
      ...spending,
      spendingScriptAddress: mainnetAddress,
    });
  });

  it("retains nullary scaffold loading and the distinct isolated always-succeeds identity", () => {
    const blueprint = readBlueprint(alwaysSucceedsBlueprintPath);
    const scaffold = makeAlwaysSucceedsContracts(blueprint);
    const isolated = makeIsolatedAlwaysSucceedsAuthenticatedValidator();
    expect(isolated.policyId).toBe(hash);
    expect(isolated.spendingScriptAddress).toBe(preprodAddress);
    expect(scaffold.stateQueue.policyId).not.toBe(isolated.policyId);
    expect(scaffold.stateQueue.spendingScriptAddress).not.toBe(
      isolated.spendingScriptAddress,
    );
    expect(scaffold.stateQueue.spendingScriptCBOR).toBe(
      applyDoubleCborEncoding(
        getCompiledScript(blueprint, "midgard.state_queue_spend.else"),
      ),
    );
  });
});
