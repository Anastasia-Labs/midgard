import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  applyParamsToScript,
  Constr,
  type Data,
  mintingPolicyToId,
  validatorToAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildDepositValidators,
  buildHubOracleMintingValidator,
  buildTxOrderValidators,
  buildWithdrawalValidators,
  type FaultProofBlueprint,
  HUB_ORACLE_ASSET_NAME,
  parseFaultProofBlueprint,
  USER_EVENT_CONTRACT_TITLES,
} from "../src/index.js";

const blueprint = parseFaultProofBlueprint(
  JSON.parse(
    readFileSync(
      process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
        fileURLToPath(
          new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        ),
      "utf8",
    ),
  ) as unknown,
);
const oneShotOutRef = { txHash: "00".repeat(32), outputIndex: 0 };
const titles = USER_EVENT_CONTRACT_TITLES;
const independentlyApply = (title: string, parameters: Data[]): string => {
  const entry = blueprint.validators.find(
    (candidate) => candidate.title === title,
  );
  if (entry === undefined)
    throw new Error(`Missing real blueprint title ${title}`);
  return parameters.length === 0
    ? entry.compiledCode
    : applyParamsToScript(entry.compiledCode, parameters);
};
const mint = (script: string) => ({ type: "PlutusV3" as const, script });
const independentlyDerivedHub = independentlyApply(titles.hubOracle.mint, [
  new Constr(0, [oneShotOutRef.txHash, 0n]),
  HUB_ORACLE_ASSET_NAME,
]);
const hubOraclePolicyId = mintingPolicyToId(mint(independentlyDerivedHub));
const input = { blueprint, network: "Preprod" as const, hubOraclePolicyId };

describe("shared user-event contract deployment recipes", () => {
  it("matches independent Lucid applications for every script, hash and spending address", () => {
    expect(
      buildHubOracleMintingValidator({ blueprint, oneShotOutRef }),
    ).toEqual({
      mintingScriptCBOR: independentlyDerivedHub,
      mintingScript: mint(independentlyDerivedHub),
      policyId: hubOraclePolicyId,
    });
    const family = buildTxOrderValidators(input);
    const certificate = independentlyApply(
      titles.txOrder.fieldPreimageCertificateMint,
      [],
    );
    const certificatePolicyId = mintingPolicyToId(mint(certificate));
    for (const [actual, mintTitle, spendTitle, parameters] of [
      [
        buildDepositValidators(input),
        titles.deposit.mint,
        titles.deposit.spend,
        [hubOraclePolicyId],
      ],
      [
        buildWithdrawalValidators(input),
        titles.withdrawal.mint,
        titles.withdrawal.spend,
        [hubOraclePolicyId],
      ],
      [
        family.txOrder,
        titles.txOrder.mint,
        titles.txOrder.spend,
        [hubOraclePolicyId, certificatePolicyId],
      ],
    ] as const) {
      const expectedMint = independentlyApply(mintTitle, [...parameters]);
      const expectedSpend = independentlyApply(spendTitle, [hubOraclePolicyId]);
      expect(actual).toEqual({
        mintingScriptCBOR: expectedMint,
        mintingScript: mint(expectedMint),
        policyId: mintingPolicyToId(mint(expectedMint)),
        spendingScriptCBOR: expectedSpend,
        spendingScript: mint(expectedSpend),
        spendingScriptHash: validatorToScriptHash(mint(expectedSpend)),
        spendingScriptAddress: validatorToAddress(
          "Preprod",
          mint(expectedSpend),
        ),
      });
    }
    expect(family.fieldPreimageCertificate.mintingScriptCBOR).toBe(certificate);
    expect(family.fieldPreimageCertificate.policyId).toBe(certificatePolicyId);
    for (const [actual, title] of [
      [
        family.fieldPreimageCertificate,
        titles.txOrder.fieldPreimageCertificateSpend,
      ],
      [family.cekProgramMaterial, titles.txOrder.cekProgramMaterialSpend],
    ] as const) {
      const expected = independentlyApply(title, []);
      expect(actual.spendingScriptCBOR).toBe(expected);
      expect(actual.spendingScriptHash).toBe(
        validatorToScriptHash(mint(expected)),
      );
      expect(actual.spendingScriptAddress).toBe(
        validatorToAddress("Preprod", mint(expected)),
      );
    }
  });

  it.each([
    "missing",
    "duplicate",
    "renamed",
    "reordered",
    "extra",
    "invalid-code",
  ] as const)("refuses a %s declared recipe", (mutation) => {
    const target = titles.txOrder.mint;
    const entry = blueprint.validators.find(
      (candidate) => candidate.title === target,
    )!;
    const changed: FaultProofBlueprint = {
      validators:
        mutation === "missing"
          ? blueprint.validators.filter(
              (candidate) => candidate.title !== target,
            )
          : mutation === "duplicate"
            ? [...blueprint.validators, entry]
            : blueprint.validators.map((candidate) =>
                candidate.title !== target
                  ? candidate
                  : {
                      ...candidate,
                      compiledCode:
                        mutation === "invalid-code"
                          ? "0g"
                          : candidate.compiledCode,
                      parameters:
                        mutation === "renamed"
                          ? candidate.parameters.map((parameter) => ({
                              ...parameter,
                              title: "wrong",
                            }))
                          : mutation === "reordered"
                            ? [...candidate.parameters].reverse()
                            : mutation === "extra"
                              ? [...candidate.parameters, { title: "extra" }]
                              : candidate.parameters,
                    },
              ),
    };
    expect(() =>
      buildTxOrderValidators({ ...input, blueprint: changed }),
    ).toThrow();
  });

  it.each(["", "00", "00".repeat(27), "gg".repeat(28)])(
    "refuses malformed hub policy %s",
    (policyId) => {
      for (const build of [
        buildDepositValidators,
        buildWithdrawalValidators,
        buildTxOrderValidators,
      ]) {
        expect(() => build({ ...input, hubOraclePolicyId: policyId })).toThrow(
          "exactly 28 bytes",
        );
      }
    },
  );

  it.each([
    { txHash: "00", outputIndex: 0 },
    { txHash: oneShotOutRef.txHash, outputIndex: -1 },
    { txHash: oneShotOutRef.txHash, outputIndex: 0.5 },
  ])("refuses an invalid one-shot outref", (oneShotOutRef) => {
    expect(() =>
      buildHubOracleMintingValidator({ blueprint, oneShotOutRef }),
    ).toThrow();
  });

  it("refuses parameters on a parameterless certificate", () => {
    const changed = {
      validators: blueprint.validators.map((entry) =>
        entry.title === titles.txOrder.fieldPreimageCertificateMint
          ? { ...entry, parameters: [{ title: "unexpected" }] }
          : entry,
      ),
    };
    expect(() =>
      buildTxOrderValidators({ ...input, blueprint: changed }),
    ).toThrow("unexpected declared parameters");
  });
});
