import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  PROTOCOL_PARAMETERS_DEFAULT,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  buildPhasMembershipRewardRegistrationTxProgram,
  parsePhasMembershipBlueprint,
  PHAS_MEMBERSHIP_WITHDRAWAL_VALIDATOR_TITLE,
  phasMembershipIdentity,
  phasMembershipWithdrawalScriptFromBlueprint,
  UnspecifiedNetworkError,
} from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(moduleDir, "../../..");
const realBlueprintPath = resolve(repoRoot, "onchain/aiken/plutus.json");

const EMULATOR_PROTOCOL_PARAMETERS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  maxTxSize: PROTOCOL_PARAMETERS_DEFAULT.maxTxSize,
  maxCollateralInputs: 3,
} as const;

const loadRealPhasMembershipScript = () =>
  phasMembershipWithdrawalScriptFromBlueprint(
    parsePhasMembershipBlueprint(
      JSON.parse(readFileSync(realBlueprintPath, "utf8")),
    ),
  );

const initEmulatorLucid = async () => {
  const account = generateEmulatorAccount({
    lovelace: 30_000_000_000n,
  });
  const emulator = new Emulator([account], EMULATOR_PROTOCOL_PARAMETERS);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  return lucid;
};

describe("PHAS membership SDK boundary", () => {
  it("parses the real blueprint and extracts the PHAS withdrawal script", () => {
    const script = loadRealPhasMembershipScript();
    expect(script.type).toBe("PlutusV3");
    expect(script.script.length).toBeGreaterThan(0);
  });

  it("rejects malformed PHAS membership blueprints", () => {
    expect(() => parsePhasMembershipBlueprint(null)).toThrow(
      "must be a JSON object",
    );
    expect(() => parsePhasMembershipBlueprint({})).toThrow(
      "must contain validators[]",
    );
    expect(() => parsePhasMembershipBlueprint({ validators: [null] })).toThrow(
      "validators[0] must be an object",
    );
    expect(() =>
      parsePhasMembershipBlueprint({
        validators: [{ title: 42, compiledCode: "5900" }],
      }),
    ).toThrow("validators[0].title must be a string");
    expect(() =>
      parsePhasMembershipBlueprint({
        validators: [{ title: "x", compiledCode: "" }],
      }),
    ).toThrow("validators[0].compiledCode must be a non-empty string");
    expect(() =>
      phasMembershipWithdrawalScriptFromBlueprint(
        parsePhasMembershipBlueprint({ validators: [] }),
      ),
    ).toThrow(PHAS_MEMBERSHIP_WITHDRAWAL_VALIDATOR_TITLE);
    expect(() =>
      phasMembershipWithdrawalScriptFromBlueprint(
        parsePhasMembershipBlueprint({
          validators: [
            {
              title: PHAS_MEMBERSHIP_WITHDRAWAL_VALIDATOR_TITLE,
              compiledCode: "5900",
            },
            {
              title: PHAS_MEMBERSHIP_WITHDRAWAL_VALIDATOR_TITLE,
              compiledCode: "5901",
            },
          ],
        }),
      ),
    ).toThrow("Expected exactly one");
  });

  it("derives the canonical PHAS membership reward identity", () => {
    const identity = phasMembershipIdentity(
      "Preprod",
      loadRealPhasMembershipScript(),
    );

    expect(identity.scriptHash).toEqual(
      "1fc59ff54da02f2535d64b40b647a8826c8b3d914d7ba5257f5b2721",
    );
    expect(identity.rewardAddress.startsWith("stake_test")).toBe(true);
  });

  it("builds PHAS registration as a script stake certificate without a Plutus certificate witness", async () => {
    const lucid = await initEmulatorLucid();
    const script = loadRealPhasMembershipScript();
    const built = await Effect.runPromise(
      buildPhasMembershipRewardRegistrationTxProgram(lucid, { script }),
    );
    const tx = CML.Transaction.from_cbor_hex(built.tx.toCBOR());
    const certs = tx.body().certs();

    expect(certs?.len()).toBe(1);
    const cert = certs!.get(0);
    expect(cert.kind()).toBe(CML.CertificateKind.StakeRegistration);
    const stakeRegistration = cert.as_stake_registration();
    expect(stakeRegistration).not.toBeUndefined();
    const credential = stakeRegistration!.stake_credential();
    expect(credential.kind()).toBe(CML.CredentialKind.Script);
    expect(credential.as_script()?.to_hex()).toBe(built.scriptHash);
    expect(built.rewardAddress.startsWith("stake_test")).toBe(true);

    const witnessSet = tx.witness_set();
    expect(witnessSet.plutus_v1_scripts()).toBeUndefined();
    expect(witnessSet.plutus_v2_scripts()).toBeUndefined();
    expect(witnessSet.plutus_v3_scripts()).toBeUndefined();
    expect(witnessSet.redeemers()).toBeUndefined();
  });

  it("fails before transaction construction when Lucid has no configured network", async () => {
    const newTx = vi.fn(() => {
      throw new Error("newTx should not be called");
    });
    const result = await Effect.runPromise(
      Effect.either(
        buildPhasMembershipRewardRegistrationTxProgram(
          {
            config: () => ({}),
            newTx,
          } as unknown as LucidEvolution,
          { script: loadRealPhasMembershipScript() },
        ),
      ),
    );

    expect(result._tag).toBe("Left");
    if (result._tag === "Left") {
      expect(result.left).toBeInstanceOf(UnspecifiedNetworkError);
    }
    expect(newTx).not.toHaveBeenCalled();
  });
});
