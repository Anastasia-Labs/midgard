import {
  generatePrivateKey,
  generateSeedPhrase,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { assertUserCliWalletIsOperationallyIsolated } from "../src/commands/cli-runtime.js";
import { assertDaBondPayerIsDedicated } from "../src/commands/da-bond.load-da-bond-context.js";
import {
  assertCommandPayerIsDedicated,
  OperationalWalletPayerRefusedError,
} from "../src/commands/operational-wallet-refusal.js";

const network = "Preprod" as const;
const main = generateSeedPhrase();
const merge = generateSeedPhrase();
const refs = generateSeedPhrase();
const deploy = generateSeedPhrase();
const env = {
  L1_OPERATOR_SEED_PHRASE: main,
  L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX: merge,
  L1_REFERENCE_SCRIPT_SEED_PHRASE: refs,
};
const deployAddress = walletFromSeed(deploy, { network }).address;
const enterprise = (seed: string) =>
  walletFromSeed(seed, { network, addressType: "Enterprise" }).address;

const check = (walletSeedEnv: string, payerAddress: string) => () =>
  assertCommandPayerIsDedicated({
    command: "availability",
    walletSeedEnv,
    payerAddress,
    referenceScriptDeployAddress: deployAddress,
    network,
    env,
  });

const refusal = (roles: readonly string[]) =>
  expect.objectContaining({
    name: "OperationalWalletPayerRefusedError",
    reason: "payer_is_operational_wallet",
    roles,
  });

describe("commands refuse the node's operational wallet as payer", () => {
  it("accepts a dedicated payer seed and wallet", () => {
    expect(
      check("CHALLENGER_SEED", enterprise(generateSeedPhrase())),
    ).not.toThrow();
  });

  it("refuses a payer seed read from one of the node's own seed settings", () => {
    expect(
      check("L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX", enterprise(merge)),
    ).toThrow(refusal(["operator-merge"]));
  });

  it("refuses an operational wallet by payment credential, whatever its address form", () => {
    // The node's wallets are base addresses; the command pays from the
    // enterprise address of the same key.
    expect(check("CHALLENGER_SEED", enterprise(main))).toThrow(
      refusal(["operator-main"]),
    );
    expect(check("CHALLENGER_SEED", enterprise(refs))).toThrow(
      refusal(["reference-scripts"]),
    );
    expect(check("CHALLENGER_SEED", enterprise(deploy))).toThrow(
      refusal(["reference-script-deploy"]),
    );
    expect(check("CHALLENGER_SEED", enterprise(main))).toThrow(
      OperationalWalletPayerRefusedError,
    );
    expect(check("CHALLENGER_SEED", enterprise(main))).toThrow(
      /availability refuses the node's operational wallet as payer/,
    );
  });

  it("da-bond top-up: a dedicated mnemonic or payment key passes, an operational one refuses", () => {
    const daBond = (walletSeedEnv: string, walletSecret: string) => () =>
      assertDaBondPayerIsDedicated({
        walletSeedEnv,
        walletSecret,
        referenceScriptDeployAddress: deployAddress,
        network,
        env,
      });
    expect(daBond("BOND_SEED", generateSeedPhrase())).not.toThrow();
    expect(daBond("BOND_SEED", generatePrivateKey())).not.toThrow();
    expect(daBond("BOND_SEED", main)).toThrow(refusal(["operator-main"]));
    expect(daBond("L1_REFERENCE_SCRIPT_SEED_PHRASE", refs)).toThrow(
      refusal(["reference-scripts"]),
    );
  });

  it("user CLI commands: a distinct wallet passes, one sharing an operational key refuses", () => {
    const isolated = (walletAddress: string) => () =>
      assertUserCliWalletIsOperationallyIsolated({
        commandName: "submit-deposit",
        walletAddress,
        operatorMainAddress: walletFromSeed(main, { network }).address,
        operatorMergeAddress: walletFromSeed(merge, { network }).address,
        referenceScriptsAddress: walletFromSeed(refs, { network }).address,
      });
    expect(
      isolated(walletFromSeed(generateSeedPhrase(), { network }).address),
    ).not.toThrow();
    expect(isolated(walletFromSeed(merge, { network }).address)).toThrow(
      refusal(["operator-merge"]),
    );
    expect(isolated(enterprise(refs))).toThrow(refusal(["reference-scripts"]));
  });
});
