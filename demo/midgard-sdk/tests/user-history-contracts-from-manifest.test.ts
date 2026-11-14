/**
 * A user's contracts come from the deployment manifest's own script CBOR.
 * The manifest here describes a deployed emulator scene exactly, so the
 * contracts it yields are checked by landing a deposit with them.
 */
import { type DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  submitDepositProgram,
  userHistoryContractsFromDeploymentManifest,
  type UserSubmissionAttempt,
} from "@al-ft/midgard-sdk";
import {
  loadUserHistoryBlueprint,
  setupUserHistoryScene,
  userHistoryManifestSections,
  type UserHistoryScene,
} from "@al-ft/midgard-sdk/test-support/user-history";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

const SCENE_TIMEOUT_MS = 300_000;

type Sections = ReturnType<typeof userHistoryManifestSections>;

const manifestOf = (sections: Sections): DeploymentManifest =>
  ({
    ...sections,
    manifestId: "ee".repeat(32),
  }) as unknown as DeploymentManifest;

describe("userHistoryContractsFromDeploymentManifest", () => {
  let scene: UserHistoryScene;
  let sections: Sections;
  beforeAll(async () => {
    scene = await setupUserHistoryScene({
      blueprint: loadUserHistoryBlueprint(),
    });
    sections = userHistoryManifestSections(scene);
  }, SCENE_TIMEOUT_MS);

  it(
    "yields the deployed contracts and the minting reference-script outrefs, and a deposit built from them lands",
    async () => {
      const { contracts, referenceScripts } =
        userHistoryContractsFromDeploymentManifest(manifestOf(sections));

      expect(referenceScripts).toEqual({
        depositMinting: {
          txHash: scene.referenceScripts.depositMinting.txHash,
          outputIndex: scene.referenceScripts.depositMinting.outputIndex,
        },
        withdrawalMinting: {
          txHash: scene.referenceScripts.withdrawalMinting.txHash,
          outputIndex: scene.referenceScripts.withdrawalMinting.outputIndex,
        },
      });
      const [depositMinting] = await scene.lucid.utxosByOutRef([
        referenceScripts.depositMinting,
      ]);
      const attempts: UserSubmissionAttempt[] = [];
      const outcome = await Effect.runPromise(
        submitDepositProgram(
          scene.view,
          contracts,
          {
            l2Address: scene.user.address,
            l2Datum: null,
            lovelace: 10_000_000n,
            additionalAssets: {},
            referenceScripts: { depositMinting: depositMinting! },
          },
          {
            store: {
              latest: async () => attempts.at(-1),
              append: async (attempt) => {
                attempts.push(attempt);
                return "created";
              },
            },
            deadlineMs: scene.emulator.now() + 3_600_000,
            ...scene.clock,
          },
        ),
      );

      expect(
        (await scene.emulator.getTransactionStatus(outcome.admissionTxHash))
          .status,
      ).toBe("confirmed");
    },
    SCENE_TIMEOUT_MS,
  );

  it("refuses a list recipe whose initialization nonce is not the manifest's hub one-shot", () => {
    const moved = {
      ...sections,
      hubOracleOneShot: {
        ...sections.hubOracleOneShot,
        outputIndex: sections.hubOracleOneShot.outputIndex + 1,
      },
    };

    expect(() =>
      userHistoryContractsFromDeploymentManifest(manifestOf(moved)),
    ).toThrow(/Manifest deposit history recipe differs from its deployment/);
  });

  it("refuses a script whose bytes do not hash to the hash the manifest records for it", () => {
    const swapped = {
      ...sections,
      contracts: {
        ...sections.contracts,
        depositSpend: {
          ...sections.contracts.depositSpend!,
          contract: sections.contracts.withdrawalSpend!.contract,
        },
      },
    };

    expect(() =>
      userHistoryContractsFromDeploymentManifest(manifestOf(swapped), {
        sourcePath: "fixture.json",
      }),
    ).toThrow(
      /Deployment manifest at "fixture.json" has invalid contracts.depositSpend.scriptHash/,
    );
  });

  it("refuses a manifest that does not record the withdrawal minting reference script", () => {
    const { "withdrawal minting": _omitted, ...rest } =
      sections.referenceScripts;

    expect(() =>
      userHistoryContractsFromDeploymentManifest(
        manifestOf({ ...sections, referenceScripts: rest }),
      ),
    ).toThrow(/does not record the withdrawal minting reference script/);
  });

  it("refuses a minting reference script recorded under another script's hash", () => {
    const foreign = {
      ...sections,
      referenceScripts: {
        ...sections.referenceScripts,
        "deposit minting": {
          ...sections.referenceScripts["deposit minting"]!,
          scriptHash: scene.pair.withdrawal.list.policyId,
        },
      },
    };

    expect(() =>
      userHistoryContractsFromDeploymentManifest(manifestOf(foreign)),
    ).toThrow(
      /records a deposit minting reference script whose hash is not contracts.depositMint.scriptHash/,
    );
  });
});
