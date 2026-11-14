/**
 * `submit-reference-input-no-idx-step-04` from files: the command resolves the field
 * grammar certificate from the deployment info and blueprint it loads, so a
 * field past the walked count limit is counted through it, and it hands the
 * step the producer's compact witness set from `--witness-set-compact`. A
 * deployment info without the certificate members fails, naming them.
 */
import { encodeMidgardTxOutputCanonical } from "@al-ft/midgard-sdk";
import { beforeAll, describe, expect, it, vi } from "vitest";

import { submitReferenceInputNoIdxStep04FromFiles } from "../src/submit-reference-input-no-idx-step-04.submit-reference-input-no-idx-step04-from-files.js";
import { deployFieldGrammarCertificate } from "./support/field-grammar-certificate-deployment-fixture.js";
import { writeNoIdxStep04Files } from "./support/no-idx-step04-from-files.js";
import {
  buildReferenceInputNoIdxBlockFixture,
  makeReferenceInputNoIdxEmulatorHarness,
} from "./support/reference-input-no-idx-emulator.js";
import {
  buildRemovalDeploymentInfo,
  network,
} from "./support/submit-init-emulator-shared.js";

const hooks = vi.hoisted(() => ({ lucid: undefined as unknown }));
// The command's provider is the emulator; the step itself is observed, not
// run, so the test pins what the command hands it.
vi.mock("../src/runtime.make-lucid-for-submit.js", async (load) => ({
  ...(await load<typeof import("../src/runtime.make-lucid-for-submit.js")>()),
  makeLucidForSubmit: async () => hooks.lucid,
}));
vi.mock(
  "../src/submit-reference-input-no-idx-step-04.submit-reference-input-no-idx-step04.js",
  () => ({
    submitReferenceInputNoIdxStep04: vi.fn(async () => ({ txHash: "00" })),
  }),
);
const { submitReferenceInputNoIdxStep04 } = await import(
  "../src/submit-reference-input-no-idx-step-04.submit-reference-input-no-idx-step04.js"
);

// Public BIP39 vector: the command resolves a signer, nothing signs.
const TEST_ONLY_SEED =
  "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
const LABEL = "submit-reference-input-no-idx-step-04";

const scenario = async () => {
  const harness = await makeReferenceInputNoIdxEmulatorHarness();
  hooks.lucid = harness.proverLucid;
  const fixture = await buildReferenceInputNoIdxBlockFixture({
    producingOutputCount: 2,
  });
  const grammar = await deployFieldGrammarCertificate({
    lucid: harness.funderLucid,
    blueprint: harness.realBlueprint,
    network,
    fieldPreimageCertificatePolicyId:
      harness.contracts.fieldPreimageCertificate.policyId,
    label: LABEL,
  });
  const files = (withMembers: boolean) =>
    writeNoIdxStep04Files({
      blueprint: harness.realBlueprint,
      deploymentInfo: buildRemovalDeploymentInfo(
        harness.contracts,
        harness.catalogue,
      ),
      ...(withMembers
        ? {
            fieldGrammarMembers: {
              referenceScriptUtxo: grammar.referenceScriptUtxo,
              spendScriptHash:
                grammar.referenceScriptsByContract.fieldGrammarCertificateSpend
                  .scriptHash,
              policyId: grammar.policyId,
            },
          }
        : {}),
      outputsPreimageCbor: fixture.producingOutputs.map((output) =>
        encodeMidgardTxOutputCanonical(output).toString("hex"),
      ),
      nativeTxCompactCbor: fixture.producingTxInclusion.nativeTxCompactCbor,
      witnessSetCompactCbor: fixture.producingWitnessSetCompactCbor,
    });
  const run = async (withMembers: boolean) =>
    await submitReferenceInputNoIdxStep04FromFiles({
      ...(await files(withMembers)),
      network,
      walletSeedPhrase: TEST_ONLY_SEED,
      threadOutRef: `${"11".repeat(32)}#0`,
    });
  return { fixture, grammar, run };
};

let built: Awaited<ReturnType<typeof scenario>>;
beforeAll(async () => {
  built = await scenario();
}, 600_000);

describe("submit-reference-input-no-idx-step-04 from files and the field grammar certificate", () => {
  it("hands the step the certificate deployment and the producer's witness set", async () => {
    vi.mocked(submitReferenceInputNoIdxStep04).mockClear();
    await built.run(true);
    expect(submitReferenceInputNoIdxStep04).toHaveBeenCalledTimes(1);
    const [call] = vi.mocked(submitReferenceInputNoIdxStep04).mock.calls[0]!;
    expect(call.fieldGrammar).toEqual(built.grammar.fieldGrammar);
    expect(call.witnessSetCompactCbor).toBe(
      built.fixture.producingWitnessSetCompactCbor,
    );
  });

  it("fails naming the certificate members when the deployment info lacks them", async () => {
    vi.mocked(submitReferenceInputNoIdxStep04).mockClear();
    await expect(built.run(false)).rejects.toThrow(
      `${LABEL} deployment lacks the field grammar certificate members fieldGrammarCertificateSpend and fieldGrammarCertificateMint`,
    );
    expect(submitReferenceInputNoIdxStep04).not.toHaveBeenCalled();
  });
});
