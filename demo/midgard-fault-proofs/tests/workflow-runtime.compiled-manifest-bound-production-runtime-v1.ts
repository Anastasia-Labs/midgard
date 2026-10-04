import { createHash } from "node:crypto";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  Lucid,
  type UTxO,
  utxoToCore,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { DaLibp2pRetainedDaSource } from "../src/transition-trace/fetch.js";
import {
  assertWorkflowJournalActuation,
  bindWorkflowActuationJournal,
  isWorkflowActuationRevokedError,
  type WorkflowActuationCheckpoint,
  workflowActuationDecisionDigest,
  WorkflowActuationRevokedError,
} from "../src/workflow/actuation-permit.js";
import {
  assertWorkflowApplicationRegistry,
  installWorkflowApplicationRegistry,
  validateWorkflowAdapterCoverage,
  WORKFLOW_ADAPTER_REGISTRATIONS,
  WORKFLOW_ADAPTER_RUNNER,
  workflowAdapterRunner,
} from "../src/workflow/adapters.js";
import {
  applyFamilyApplicationRecord,
  defineFamilyApplication,
} from "../src/workflow/family-application.js";
import { FAMILY_APPLICATION_REGISTRY } from "../src/workflow/family-application-registry.js";
import {
  assertWorkflowFundingReservationReadyToSubmit,
  beginWorkflowFundingReservationAction,
  bindWorkflowFundingReservationJournal,
  confirmWorkflowFundingReservationTransaction,
  releaseIdleWorkflowFundingReservation,
  reobserveWorkflowFundingReservationTransaction,
  restrictWorkflowFundingSigner,
  unsafeWorkflowFundingReservationSelectedOutRefsForTest,
  type WorkflowFundingReservationSnapshot,
  WorkflowFundingReservationUnavailableError,
} from "../src/workflow/funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  MemoryFraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import { continuePendingWorkflow } from "../src/workflow/pending-continuation.js";
import {
  createFamilyApplicationWorkflowRunner,
  createManifestBoundWorkflowRunner,
  type LoadedWorkflowRuntime,
  WORKFLOW_RUNNER_FACTORIES,
} from "../src/workflow/runtime.js";
import * as runtime from "../src/workflow/runtime.js";
import { readWorkflowRuntimeFundingPolicy } from "../src/workflow/runtime-funding-policy.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { familyCommonInfrastructureForTest } from "./support/family-common-infrastructure.js";
import {
  admittedActuation,
  DEPLOYMENT,
  fundingAddress,
  fundingKey,
  fundingReferenceOutRef,
  fundingReferenceScript,
} from "./workflow-runtime.admitted-actuation.js";
import { assertOrdinaryMissingChange } from "./workflow-runtime.ordinary-change-control.js";
import {
  retainedDaSource,
  runtimeFunding,
  runtimeFundingSelection,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";
import {
  type DoubleSpendIdentity,
  doubleSpendRecord,
  loadedRuntime,
  prepareRuntimeFunding,
  slashFundingFixture,
} from "./workflow-runtime.slash-funding-fixture.js";

describe("compiled manifest-bound production runtime V1", () => {
  it(
    "requires reserved-wallet change for ordinary funding",
    assertOrdinaryMissingChange,
  );
  it.each(["full", "partially-inactivity-slashed"] as const)(
    "admits exact signed %s slash economics without spending ordinary wallet funds",
    async (tranche) => {
      const runtime = await slashFundingFixture({ tranche });
      try {
        await runtime.admit();
        expect(runtime.protocolAuthority).toHaveBeenCalledTimes(2);
        expect(runtime.prepare).toHaveBeenCalledOnce();
        expect(runtime.prepare).toHaveBeenCalledWith(
          expect.objectContaining({
            transition: expect.objectContaining({
              actionKind: "remove",
              consumedOutRefs: [],
              signedTransactionCborHex: runtime.signed
                .toTransaction()
                .to_cbor_hex(),
              producedInputs: [
                expect.objectContaining({ lovelace: "400000000" }),
              ],
            }),
          }),
        );
        const policy = readWorkflowRuntimeFundingPolicy(runtime.policy);
        expect(policy.maximumSlashCollateralLovelace).toBe("750000000");
        expect(BigInt(policy.maximumFeeLovelace)).toBeLessThan(400_000_000n);
        expect(BigInt(policy.maximumCollateralLovelace)).toBeLessThan(
          600_000_000n,
        );
        await expect(
          assertWorkflowFundingReservationReadyToSubmit({
            journal: runtime.journal,
            transactionHash: runtime.signed.toHash(),
          }),
        ).resolves.toBeUndefined();
      } finally {
        runtime.close();
      }
    },
  );
  it.each([
    { label: "fee below tranche", feeDelta: -1n },
    { label: "fee above tranche", feeDelta: 1n },
    { label: "reward below release", rewardDelta: -1n },
    { label: "reward above release", rewardDelta: 1n },
    { label: "different action", actionStage: "step-one" },
    { label: "missing private capability", capability: false },
    { label: "ordinary high fee", capability: false, actionStage: "step-one" },
    { label: "ordinary wallet input", includeWalletInput: true },
    { label: "foreign protocol authority", foreignAuthority: true },
  ])(
    "rejects signed slash funding with $label before durable preparation",
    async (options) => {
      const runtime = await slashFundingFixture(options);
      try {
        await expect(runtime.admit()).rejects.toThrow();
        expect(runtime.prepare).not.toHaveBeenCalled();
      } finally {
        runtime.close();
      }
    },
  );
  it("rechecks the exact signed slash bytes before submission", async () => {
    const runtime = await slashFundingFixture();
    try {
      await runtime.admit();
      const changed = signedFundingTransaction({
        inputOutRefs: [],
        outputLovelace: 400_000_001n,
      });
      runtime.signed.toTransaction = changed.toTransaction;
      await expect(
        assertWorkflowFundingReservationReadyToSubmit({
          journal: runtime.journal,
          transactionHash: runtime.signed.toHash(),
        }),
      ).rejects.toThrow("changed before submission");
    } finally {
      runtime.close();
    }
  });
  it("prepares the exact signed wire and original body digest used by default submission", async () => {
    const runtime = await runtimeFunding("step-one");
    const signed = signedFundingTransaction({
      inputOutRefs: runtime.selected.fundingOutRefs,
      outputLovelace: 14_800_000n,
      nonCanonicalBody: true,
    });
    const transaction = signed.toTransaction();
    expect(transaction.to_cbor_hex()).not.toBe(
      transaction.to_canonical_cbor_hex(),
    );
    await prepareRuntimeFunding(runtime, signed);
    expect(runtime.prepare).toHaveBeenCalledWith(
      expect.objectContaining({
        transition: expect.objectContaining({
          signedTransactionCborHex: transaction.to_cbor_hex(),
          transactionBodySha256: createHash("sha256")
            .update(Buffer.from(transaction.body().to_cbor_hex(), "hex"))
            .digest("hex"),
        }),
      }),
    );
  });
  it("reserves the consumed signed subset while leaving unused leased candidates intact", async () => {
    const runtime = await runtimeFunding("step-one");
    const input = `${"73".repeat(32)}#0`;
    await prepareRuntimeFunding(
      runtime,
      signedFundingTransaction({
        inputOutRefs: [input],
        outputLovelace: 9_800_000n,
      }),
    );
    expect(runtime.prepare).toHaveBeenCalledWith(
      expect.objectContaining({
        transition: expect.objectContaining({ consumedOutRefs: [input] }),
      }),
    );
  });
  it("defers live input resolution until after the durable pending intent can reconcile", async () => {
    const runtime = await runtimeFunding("step-one", { begin: false });
    expect(runtime.resolveInputs).not.toHaveBeenCalled();
    await runtime.begin();
    expect(runtime.resolveInputs).toHaveBeenCalledWith(
      runtime.snapshot.activeInputs.map(({ outRef }) => outRef),
    );
  });

  it.each(["foreign-wallet", "signature", "fee", "execution-units"] as const)(
    "refuses actual signed %s before preparing any durable transition",
    async (kind) => {
      const runtime = await runtimeFunding("step-one");
      const maximumFee = BigInt(
        readWorkflowRuntimeFundingPolicy(runtime.policy).maximumFeeLovelace,
      );
      const otherKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 0x52));
      const otherAddress = CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(otherKey.to_public().hash()),
      )
        .to_address()
        .to_bech32();
      const fee = kind === "fee" ? maximumFee + 1n : 200_000n;
      const signed = signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 15_000_000n - fee,
        fee,
        ...(kind === "foreign-wallet" ? { outputAddress: otherAddress } : {}),
        ...(kind === "signature" ? { signingKey: otherKey } : {}),
        ...(kind === "execution-units" ? { redeemerMemory: 14_000_001n } : {}),
      });
      await expect(prepareRuntimeFunding(runtime, signed)).rejects.toThrow(
        kind === "foreign-wallet"
          ? "escapes"
          : kind === "signature"
            ? "signature"
            : kind === "fee"
              ? "fee"
              : "maxTxExUnits",
      );
      expect(runtime.prepare).not.toHaveBeenCalled();
    },
  );

  it("accepts exact min-Ada custody and refuses an extra wallet-funded lovelace", async () => {
    for (const surplus of [0n, 1n]) {
      const runtime = await runtimeFunding("step-one");
      const policy = readWorkflowRuntimeFundingPolicy(runtime.policy);
      const address = CML.Address.from_bech32(policy.contracts[0]!.address);
      const datum = () =>
        CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex("00"));
      const minimum = CML.min_ada_required(
        CML.TransactionOutput.new(
          address,
          CML.Value.from_coin(2_000_000n),
          datum(),
        ),
        4310n,
      );
      const allocation = minimum + surplus;
      const signed = signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 14_800_000n - allocation,
        additionalOutputs: [
          CML.TransactionOutput.new(
            address,
            CML.Value.from_coin(allocation),
            datum(),
          ),
        ],
      });
      if (surplus === 0n)
        await expect(
          prepareRuntimeFunding(runtime, signed),
        ).resolves.toBeUndefined();
      else
        await expect(prepareRuntimeFunding(runtime, signed)).rejects.toThrow(
          "custody allocation",
        );
    }
  });

  it("requires exact confirmed lineage before reusing locked workflow capital", async () => {
    const sample = await runtimeFunding("step-one");
    const address = readWorkflowRuntimeFundingPolicy(sample.policy)
      .contracts[0]!.address;
    const locked: UTxO = {
      txHash: "77".repeat(32),
      outputIndex: 0,
      address,
      assets: { lovelace: 3_000_000n },
      datum: "00",
    };
    for (const lineage of ["exact", "missing", "changed"] as const) {
      const runtime = await runtimeFunding("step-one", {
        additionalInputs: [locked],
        ...(lineage === "missing" ? {} : { confirmedInput: locked }),
        changedLineage: lineage === "changed",
      });
      const signed = signedFundingTransaction({
        inputOutRefs: [
          ...runtime.selected.fundingOutRefs,
          `${locked.txHash}#0`,
        ],
        outputLovelace: 14_800_000n,
        additionalOutputs: [utxoToCore(locked).output()],
      });
      if (lineage === "exact")
        await expect(
          prepareRuntimeFunding(runtime, signed),
        ).resolves.toBeUndefined();
      else
        await expect(prepareRuntimeFunding(runtime, signed)).rejects.toThrow(
          "lineage",
        );
    }
  });

  it("keeps the collateral availability floor separate from exact failure forfeiture", async () => {
    const runtime = await runtimeFunding("step-one", { collateral: true });
    await expect(
      prepareRuntimeFunding(
        runtime,
        signedFundingTransaction({
          inputOutRefs: runtime.selected.fundingOutRefs,
          outputLovelace: 14_800_000n,
          redeemerMemory: 1n,
          collateral: {
            outRefs: runtime.selected.collateralOutRefs,
            total: 300_000n,
            returned: 4_700_000n,
          },
        }),
      ),
    ).resolves.toBeUndefined();
    const insufficient = await runtimeFunding("step-one", { collateral: true });
    await expect(
      prepareRuntimeFunding(
        insufficient,
        signedFundingTransaction({
          inputOutRefs: insufficient.selected.fundingOutRefs,
          outputLovelace: 14_800_000n,
          redeemerMemory: 1n,
          collateral: {
            outRefs: insufficient.selected.collateralOutRefs,
            total: 299_999n,
            returned: 4_700_001n,
          },
        }),
      ),
    ).rejects.toThrow("collateral");
  });

  it("uses the unchanged journal stage to prepare actual signed funding", async () => {
    const runtime = await runtimeFunding("verify_source", {
      useStage: true,
    });
    const preflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "stage-funded" }),
      signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 14_800_000n,
      }),
    );
    await expect(
      runtime.prepareTransaction({
        action: {
          actionId: "verify_source",
          input: { stage: "verify_source" },
        },
        preflight,
      }),
    ).resolves.toBeUndefined();
    expect(runtime.prepare).toHaveBeenCalledTimes(1);
  });

  it("binds each funding reservation permit to exactly one workflow journal", async () => {
    const authority = await admittedActuation();
    const first = Object.freeze({ id: "first" });
    const second = Object.freeze({ id: "second" });
    expect(
      bindWorkflowFundingReservationJournal({
        journal: first,
        permit: authority.fundingReservationPermit,
      }),
    ).toBe(first);
    expect(() =>
      bindWorkflowFundingReservationJournal({
        journal: second,
        permit: authority.fundingReservationPermit,
      }),
    ).toThrow("already bound to a workflow journal");
  });

  it("exposes only durable leased candidates independently of action samples", async () => {
    await expect(runtimeFundingSelection("step-one")).resolves.toEqual({
      fundingOutRefs: [
        `${"71".repeat(32)}#0`,
        `${"72".repeat(32)}#0`,
        `${"73".repeat(32)}#0`,
      ],
      collateralOutRefs: [],
    });
    await expect(runtimeFundingSelection("step-three")).resolves.toEqual({
      fundingOutRefs: [
        `${"71".repeat(32)}#0`,
        `${"72".repeat(32)}#0`,
        `${"73".repeat(32)}#0`,
      ],
      collateralOutRefs: [],
    });
  });

  it("refreshes a reused wallet after publication before admitting the next proof step", async () => {
    const runtime = await runtimeFunding("step-one", {
      collateral: true,
      begin: false,
    });
    const lucid = await Lucid(new Emulator([]), "Preprod");
    const signer = restrictWorkflowFundingSigner({
      permit: runtime.permit,
      signer: {
        source: "funding-test",
        address: fundingAddress,
        paymentKeyHash: fundingKey.to_public().hash().to_hex(),
        selectWallet: (instance) =>
          instance.selectWallet.fromPrivateKey(fundingKey.to_bech32()),
      },
    });
    const publication = {
      actionId: "publish-field-carriage:step_02",
      input: { actionKind: "publish_field_carriage" },
    };
    signer.selectWallet(lucid);
    const wallet = lucid.wallet();
    const getCollateral = wallet.getCollateral;
    if (getCollateral === undefined)
      throw new Error("reserved wallet omitted collateral API");
    await expect(wallet.getUtxos()).rejects.toThrow(
      "before a reserved action began",
    );
    await expect(getCollateral()).rejects.toThrow(
      "before a reserved action began",
    );
    const premature = signedFundingTransaction({
      inputOutRefs: [],
      outputLovelace: 1_000_000n,
    });
    await expect(wallet.signTx(premature.toTransaction())).rejects.toThrow(
      "before a reserved action began",
    );
    await expect(
      wallet.submitTx(premature.toTransaction().to_cbor_hex()),
    ).rejects.toThrow("before a reserved action began");
    await beginWorkflowFundingReservationAction({
      journal: runtime.journal,
      action: publication,
    });
    const outRef = (utxo: UTxO) => `${utxo.txHash}#${utxo.outputIndex}`;
    const previousInputs = await wallet.getUtxos();
    const published = signedFundingTransaction({
      inputOutRefs: previousInputs.map(outRef),
      outputLovelace: 14_800_000n,
    });
    await runtime.prepareTransaction({
      action: publication,
      preflight: bindWorkflowPreflightTransaction(
        Object.freeze({ txHash: published.toHash() }),
        published,
      ),
    });
    const change: UTxO = {
      txHash: published.toHash(),
      outputIndex: 0,
      address: fundingAddress,
      assets: { lovelace: 14_800_000n },
    };
    const collateral = await getCollateral();
    const currentInputs = [change, ...collateral];
    runtime.resolveInputs.mockImplementation(async (refs) =>
      currentInputs.filter((utxo) => refs.includes(outRef(utxo))),
    );
    runtime.setSnapshot({
      ...runtime.snapshot,
      revision: "2",
      activeInputs: [
        {
          outRef: outRef(change),
          role: "funding" as const,
          lovelace: "14800000",
          assets: [],
        },
        ...runtime.snapshot.activeInputs.filter(
          ({ role }) => role === "collateral",
        ),
      ].sort((left, right) => left.outRef.localeCompare(right.outRef)),
    });
    await confirmWorkflowFundingReservationTransaction({
      journal: runtime.journal,
      transactionHash: published.toHash(),
    });
    await expect(wallet.getUtxos()).rejects.toThrow(
      "before a reserved action began",
    );
    await expect(getCollateral()).rejects.toThrow(
      "before a reserved action began",
    );
    const step = { actionId: "step_02", input: { stage: "step_02" } };
    const restarted = await Lucid(new Emulator([]), "Preprod");
    signer.selectWallet(restarted);
    await expect(restarted.wallet().getUtxos()).rejects.toThrow(
      "before a reserved action began",
    );
    await beginWorkflowFundingReservationAction({
      journal: runtime.journal,
      action: step,
    });
    // The linear continuation builder reuses Lucid without selecting its signer again.
    expect(lucid.wallet()).toBe(wallet);
    expect(await wallet.getUtxos()).toEqual([change]);
    expect(await restarted.wallet().getUtxos()).toEqual([change]);
    expect(await getCollateral()).toEqual(collateral);
    const signed = signedFundingTransaction({
      inputOutRefs: (await wallet.getUtxos()).map(outRef),
      outputLovelace: 14_600_000n,
    });
    await runtime.prepareTransaction({
      action: step,
      preflight: bindWorkflowPreflightTransaction(
        Object.freeze({ txHash: signed.toHash() }),
        signed,
      ),
    });
    expect(runtime.prepare).toHaveBeenLastCalledWith(
      expect.objectContaining({
        transition: expect.objectContaining({
          consumedOutRefs: [outRef(change)],
        }),
      }),
    );
    runtime.actuation.restrictToReconciliation("test rollback");
    await expect(wallet.getUtxos()).rejects.toThrow("reconciliation-only");
    await expect(getCollateral()).rejects.toThrow("reconciliation-only");
    const resumed = await Lucid(new Emulator([]), "Preprod");
    signer.selectWallet(resumed);
    await expect(resumed.wallet().getUtxos()).rejects.toThrow(
      "reconciliation-only",
    );
    await expect(
      resumed.wallet().signTx(signed.toTransaction()),
    ).rejects.toThrow("reconciliation-only");
    await expect(
      resumed.wallet().submitTx(signed.toTransaction().to_cbor_hex()),
    ).rejects.toThrow("reconciliation-only");
    await expect(
      resumed.wallet().signMessage(fundingAddress, "00"),
    ).rejects.toThrow("reconciliation-only");
  });

  it("binds the actual signed body to durable leased wallet inputs", async () => {
    const runtime = await runtimeFunding("step-one");
    const action = { actionId: "step-one", input: { actionKind: "step-one" } };
    const validPreflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "valid" }),
      signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 14_800_000n,
      }),
    );
    await expect(
      runtime.prepareTransaction({
        action,
        preflight: validPreflight,
      }),
    ).resolves.toBeUndefined();
    expect(runtime.prepare).toHaveBeenCalledTimes(1);

    const hostile = await runtimeFunding("step-one");
    const substitutedPreflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "substituted" }),
      signedFundingTransaction({
        inputOutRefs: [
          ...hostile.selected.fundingOutRefs,
          `${"75".repeat(32)}#0`,
        ],
        outputLovelace: 15_800_000n,
      }),
    );
    await expect(
      hostile.prepareTransaction({
        action,
        preflight: substitutedPreflight,
      }),
    ).rejects.toThrow("unreserved wallet input");
    expect(hostile.prepare).not.toHaveBeenCalled();
  });

  it("rejects a substituted reference script even when body topology and byte count match", async () => {
    const substitutedScript = Object.freeze({
      type: "PlutusV3" as const,
      script: "4d01000033222220051200120012",
    });
    const runtime = await runtimeFunding("step-one", {
      governedReference: fundingReferenceScript,
      resolvedReference: substitutedScript,
    });
    const preflight = bindWorkflowPreflightTransaction(
      Object.freeze({ txHash: "reference-substitution" }),
      signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 14_800_000n,
        referenceOutRefs: [fundingReferenceOutRef],
      }),
    );
    await expect(
      runtime.prepareTransaction({
        action: { actionId: "step-one", input: { actionKind: "step-one" } },
        preflight,
      }),
    ).rejects.toThrow("ungoverned reference script");
    expect(runtime.prepare).not.toHaveBeenCalled();
  });

  it("rechecks exact reserved values after durable refresh", async () => {
    const runtime = await runtimeFunding("step-three");
    runtime.setSnapshot(
      Object.freeze({
        ...runtime.snapshot,
        revision: "1",
        activeInputs: Object.freeze([
          ...runtime.snapshot.activeInputs,
          Object.freeze({
            outRef: `${"75".repeat(32)}#0`,
            role: "collateral" as const,
            lovelace: "1",
            assets: Object.freeze([]),
          }),
        ]),
      }),
    );
    await expect(
      assertWorkflowFundingReservationReadyToSubmit({
        journal: runtime.journal,
        transactionHash: "00".repeat(32),
      }),
    ).rejects.toThrow("resolver changed reserved lovelace");
  });

  it.each(["missing", "changed", "extra"] as const)(
    "keeps a prepared signed attempt when inputs disappear, but rejects %s resolver substitution",
    async (kind) => {
      const runtime = await runtimeFunding("step-one");
      const signed = signedFundingTransaction({
        inputOutRefs: runtime.selected.fundingOutRefs,
        outputLovelace: 14_800_000n,
      });
      await prepareRuntimeFunding(runtime, signed);
      const present = (
        await runtime.resolveInputs(runtime.selected.fundingOutRefs)
      ).slice(1);
      if (kind === "changed")
        present[0] = { ...present[0]!, assets: { lovelace: 1n } };
      if (kind === "extra")
        present.push({ ...present[0]!, txHash: "ab".repeat(32) });
      runtime.resolveInputs.mockResolvedValueOnce(present);
      const check = assertWorkflowFundingReservationReadyToSubmit({
        journal: runtime.journal,
        transactionHash: signed.toHash(),
      });
      if (kind === "missing")
        await expect(check).rejects.toBeInstanceOf(
          WorkflowFundingReservationUnavailableError,
        );
      else
        await expect(check).rejects.toThrow(
          kind === "changed"
            ? "changed reserved lovelace"
            : "changed the reserved input set",
        );
      expect(runtime.prepare).toHaveBeenCalledOnce();
      expect(runtime.releaseIdle).not.toHaveBeenCalled();
    },
  );

  it("admits every fixed factory only for its exact application category", () => {
    const categories = Object.keys(
      WORKFLOW_RUNNER_FACTORIES,
    ) as (keyof typeof WORKFLOW_RUNNER_FACTORIES)[];
    for (const category of categories) {
      const runner = WORKFLOW_RUNNER_FACTORIES[category](async () => {
        throw new Error(`${category} loader is not invoked during admission`);
      });
      const registry = installWorkflowApplicationRegistry({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: [category],
        installations: [
          { category, deploymentFingerprint: DEPLOYMENT, runner },
        ],
      });
      expect(
        registry.registrations.find(
          (registration) => registration.category === category,
        ),
      ).toMatchObject({ category, status: "ready", runner });
      const otherCategory = categories.find(
        (candidate) => candidate !== category,
      )!;
      expect(() =>
        installWorkflowApplicationRegistry({
          deploymentFingerprint: DEPLOYMENT,
          requiredInstalledCategories: [otherCategory],
          installations: [
            {
              category: otherCategory,
              deploymentFingerprint: DEPLOYMENT,
              runner,
            },
          ],
        }),
      ).toThrow("module-admitted category-bound runner");
    }
  });

  it("derives one factory per registry record and exports no per-family constructor", () => {
    expect(Object.keys(WORKFLOW_RUNNER_FACTORIES).sort()).toEqual(
      Object.keys(FAMILY_APPLICATION_REGISTRY).sort(),
    );
    for (const factory of Object.values(WORKFLOW_RUNNER_FACTORIES)) {
      expect(typeof factory).toBe("function");
    }
    // The only runner constructors the module exports are the two generic
    // ones: the public non-admissible body and the record-typed admitted
    // one. A family is added to the table by registering its record.
    expect(
      Object.keys(runtime)
        .filter((name) => /^create\w+WorkflowRunner$/.test(name))
        .sort(),
    ).toEqual([
      "createFamilyApplicationWorkflowRunner",
      "createManifestBoundWorkflowRunner",
    ]);
  });

  it("gives every derived runner exactly the shared drive surface", () => {
    for (const [category, factory] of Object.entries(
      WORKFLOW_RUNNER_FACTORIES,
    )) {
      const runner = factory(async () => {
        throw new Error(`${category} loader is not invoked during admission`);
      });
      expect(runner.runnerVersion).toBe(WORKFLOW_ADAPTER_RUNNER);
      expect(Object.keys(runner).sort()).toEqual([
        "runOrResume",
        "runnerVersion",
      ]);
    }
  });

  it("wires every row to the record of its own category", async () => {
    // The table is keyed by the registry, and each row's body refuses a
    // foreign category naming its own: together these pin row X to record X.
    for (const [category, factory] of Object.entries(
      WORKFLOW_RUNNER_FACTORIES,
    )) {
      const loadRuntimeConfig = vi.fn();
      const runner = factory(loadRuntimeConfig);
      const foreign = category === "doubleSpend" ? "zeroInput" : "doubleSpend";
      await expect(
        runner.runOrResume({ category: foreign } as never),
      ).rejects.toThrow(
        `production workflow runner category mismatch: expected=${category} actual=${foreign}`,
      );
      expect(loadRuntimeConfig).not.toHaveBeenCalled();
    }
  });

  it("does not admit the public generic constructor as a production family runner", () => {
    const generic = createManifestBoundWorkflowRunner({
      record: doubleSpendRecord({
        bindConfig: () => undefined,
        constructWorkflow: async (): Promise<DoubleSpendIdentity> => {
          throw new Error("generic runner is not invoked during admission");
        },
        execute: async () => {
          throw new Error("generic runner is not invoked during admission");
        },
      }),
      loadRuntime: async () => {
        throw new Error("generic runner is not invoked during admission");
      },
    });
    expect(() =>
      validateWorkflowAdapterCoverage(
        WORKFLOW_ADAPTER_REGISTRATIONS.map((registration) =>
          registration.category === "doubleSpend"
            ? { ...registration, status: "ready", runner: generic }
            : registration,
        ),
      ),
    ).toThrow("no compiled executable runner admitted for its exact category");
  });

  it("installs an immutable deployment-bound application overlay without mutating the static catalogue", () => {
    const runner = WORKFLOW_RUNNER_FACTORIES.daHashPreimage(async () => {
      throw new Error("installed Q44 loader reached");
    });
    const registry = installWorkflowApplicationRegistry({
      deploymentFingerprint: DEPLOYMENT,
      requiredInstalledCategories: ["daHashPreimage"],
      installations: [
        {
          category: "daHashPreimage",
          deploymentFingerprint: DEPLOYMENT,
          runner,
        },
      ],
    });
    expect(() => assertWorkflowApplicationRegistry(registry)).not.toThrow();
    expect(Object.isFrozen(registry)).toBe(true);
    expect(Object.isFrozen(registry.installedCategories)).toBe(true);
    expect(Object.isFrozen(registry.registrations)).toBe(true);
    expect(registry.registrations).toHaveLength(
      WORKFLOW_ADAPTER_REGISTRATIONS.length,
    );
    expect(
      registry.registrations.find(
        (registration) => registration.category === "daHashPreimage",
      ),
    ).toMatchObject({ status: "ready", runner });
    expect(
      WORKFLOW_ADAPTER_REGISTRATIONS.find(
        (registration) => registration.category === "daHashPreimage",
      ),
    ).toMatchObject({ status: "missing" });
    expect(workflowAdapterRunner("daHashPreimage", registry)).toBe(runner);
  });

  it("rejects incomplete, duplicate, unrecognized, forged, and cross-category application installations", () => {
    const doubleSpend = WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
      throw new Error("not invoked");
    });
    const daHashPreimage = WORKFLOW_RUNNER_FACTORIES.daHashPreimage(
      async () => {
        throw new Error("not invoked");
      },
    );
    const install = (
      input: Parameters<typeof installWorkflowApplicationRegistry>[0],
    ) => installWorkflowApplicationRegistry(input);

    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["doubleSpend", "daHashPreimage"],
        installations: [
          {
            category: "doubleSpend",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
        ],
      }),
    ).toThrow("installation cardinality mismatch");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["doubleSpend", "daHashPreimage"],
        installations: [
          {
            category: "doubleSpend",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
          {
            category: "doubleSpend",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
        ],
      }),
    ).toThrow("duplicates doubleSpend");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["daHashPreimage"],
        installations: [
          {
            category: "daHashPreimage",
            deploymentFingerprint: "ff".repeat(32),
            runner: daHashPreimage,
          },
        ],
      }),
    ).toThrow("unrecognized deployment identity");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["daHashPreimage"],
        installations: [
          {
            category: "daHashPreimage",
            deploymentFingerprint: DEPLOYMENT,
            runner: doubleSpend,
          },
        ],
      }),
    ).toThrow("module-admitted category-bound runner");
    expect(() =>
      install({
        deploymentFingerprint: DEPLOYMENT,
        requiredInstalledCategories: ["daHashPreimage"],
        installations: [
          {
            category: "daHashPreimage",
            deploymentFingerprint: DEPLOYMENT,
            runner: {
              runnerVersion: WORKFLOW_ADAPTER_RUNNER,
              runOrResume: async () => undefined,
            },
          },
        ],
      }),
    ).toThrow("module-admitted category-bound runner");
    expect(() =>
      assertWorkflowApplicationRegistry({
        schemaVersion: "midgard-production-fraud-proof-application-registry-v1",
        deploymentFingerprint: DEPLOYMENT,
        installedCategories: ["daHashPreimage"],
        registrations: WORKFLOW_ADAPTER_REGISTRATIONS,
      }),
    ).toThrow("not installed through the authenticated immutable boundary");
  });

  it("constructs the exact workflow and supplies a restart-durable directory journal", async () => {
    const actuation = await admittedActuation();
    const directory = await mkdtemp(join(tmpdir(), "midgard-runtime-v1-"));
    const journalDirectory = join(directory, "journal");
    const close = vi.fn(async () => undefined);
    const loadRuntime = vi.fn(async () => loadedRuntime(actuation, { close }));
    const bindConfig = vi.fn(() => ({ releaseConfig: "manifest-bound" }));
    const constructWorkflow = vi.fn(async (_config: unknown) => ({
      binding: {
        deploymentFingerprint: DEPLOYMENT,
        definition: {
          category: "doubleSpend" as const,
          headerHash: actuation.headerHash,
        },
      },
    }));
    const execute = vi.fn(async ({ journal, mode }) => {
      expect(journal).toBeInstanceOf(DirectoryFraudProofWorkflowJournalStore);
      expect(mode).toBe("resume");
      const identity: FraudProofWorkflowIdentity = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        deploymentFingerprint: DEPLOYMENT,
        category: "doubleSpend",
        target: {
          kind: "state_queue_header",
          headerHash: actuation.headerHash,
        },
        decisionDigest: actuation.decisionDigest,
      };
      const workflowId = computeFraudProofWorkflowId(identity);
      await journal.append(
        {
          schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
          workflowId,
          identity,
          sequence: 0,
          recordedAt: "2026-08-29T00:00:00.000Z",
          event: { kind: "started" },
        },
        0,
      );
      return { workflowId };
    });
    try {
      const runner = createManifestBoundWorkflowRunner({
        record: doubleSpendRecord({ bindConfig, constructWorkflow, execute }),
        loadRuntime,
      });
      expect(runner.runnerVersion).toBe(WORKFLOW_ADAPTER_RUNNER);
      const result = await runner.runOrResume({
        mode: "resume",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        fundingReservationPermit: actuation.fundingReservationPermit,
        journalDirectory,
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
      });
      expect(loadRuntime).toHaveBeenCalledWith({
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
        invocation: expect.objectContaining({
          category: "doubleSpend",
          deploymentFingerprint: DEPLOYMENT,
          headerHash: actuation.headerHash,
        }),
      });
      // The config the record binds from the loaded infrastructure is what
      // its constructor receives: the loader never hands a family its config.
      expect(bindConfig).toHaveBeenCalledWith(
        expect.objectContaining({
          infrastructure: expect.objectContaining({
            headerHash: actuation.headerHash,
          }),
          references: {},
        }),
      );
      expect(constructWorkflow).toHaveBeenCalledWith({
        releaseConfig: "manifest-bound",
      });
      expect(close).toHaveBeenCalledOnce();
      const workflowId = (result as { readonly workflowId: string }).workflowId;
      await expect(
        new DirectoryFraudProofWorkflowJournalStore(journalDirectory).load(
          workflowId,
        ),
      ).resolves.toHaveLength(1);
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });

  it("keeps one admitted runner session until pending work completes", async () => {
    const actuation = await admittedActuation();
    const close = vi.fn(async () => undefined);
    const sources = [retainedDaSource()];
    const workflow = {
      binding: {
        deploymentFingerprint: DEPLOYMENT,
        definition: {
          category: "doubleSpend" as const,
          headerHash: actuation.headerHash,
        },
      },
    };
    const loadRuntime = vi.fn(async () =>
      loadedRuntime(actuation, { retainedDaSources: sources, close }),
    );
    const constructWorkflow = vi.fn(async () => workflow);
    const completed = { kind: "completed", workflowId: "existing-workflow" };
    const execute =
      vi.fn<
        Parameters<
          typeof doubleSpendRecord<Record<string, never>, typeof workflow>
        >[0]["execute"]
      >();
    execute.mockImplementation(async () =>
      execute.mock.calls.length < 3 ? { kind: "pending" } : completed,
    );
    const runner = createManifestBoundWorkflowRunner({
      record: doubleSpendRecord({
        bindConfig: () => ({}),
        constructWorkflow,
        execute,
      }),
      loadRuntime,
    });
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      const running = runner.runOrResume({
        mode: "run",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        fundingReservationPermit: actuation.fundingReservationPermit,
        journalDirectory: "/tmp/midgard-runtime-pending-session",
        runtimeConfigPath: "/etc/midgard/runtime.json",
      });
      await vi.advanceTimersByTimeAsync(0);
      expect(execute).toHaveBeenCalledTimes(1);
      expect(close).not.toHaveBeenCalled();
      await vi.advanceTimersByTimeAsync(999);
      expect(execute).toHaveBeenCalledTimes(1);
      await vi.advanceTimersByTimeAsync(1);
      expect(execute).toHaveBeenCalledTimes(2);
      expect(close).not.toHaveBeenCalled();
      await vi.advanceTimersByTimeAsync(1_000);
      await expect(running).resolves.toBe(completed);
      expect(execute).toHaveBeenCalledTimes(3);
      const calls = execute.mock.calls;
      expect(calls.map(([input]) => input.mode)).toEqual([
        "run",
        "resume",
        "resume",
      ]);
      for (const [input] of calls) {
        expect(input.journal).toBe(calls[0]![0].journal);
        expect(input.workflow).toBe(workflow);
        expect(input.sources).toBe(sources);
      }
      expect(loadRuntime).toHaveBeenCalledOnce();
      expect(constructWorkflow).toHaveBeenCalledOnce();
      expect(close).toHaveBeenCalledOnce();
    } finally {
      vi.useRealTimers();
    }
  });

  it("stops pending continuation when authority is revoked and closes the session", async () => {
    const actuation = await admittedActuation();
    const close = vi.fn(async () => undefined);
    const sources = [retainedDaSource()];
    const execute = vi.fn(async () => ({ kind: "pending" }));
    const runner = createManifestBoundWorkflowRunner({
      record: doubleSpendRecord({
        bindConfig: () => ({}),
        constructWorkflow: async () => ({
          binding: {
            deploymentFingerprint: DEPLOYMENT,
            definition: {
              category: "doubleSpend" as const,
              headerHash: actuation.headerHash,
            },
          },
        }),
        execute,
      }),
      loadRuntime: async () =>
        loadedRuntime(actuation, { retainedDaSources: sources, close }),
    });
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      const running = runner.runOrResume({
        mode: "resume",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        fundingReservationPermit: actuation.fundingReservationPermit,
        journalDirectory: "/tmp/midgard-runtime-pending-revocation",
        runtimeConfigPath: "/etc/midgard/runtime.json",
      });
      const rejected = running.catch((error: unknown) => error);
      await vi.advanceTimersByTimeAsync(0);
      expect(execute).toHaveBeenCalledTimes(1);
      actuation.revoke("canonical rollback observed during confirmation");
      await vi.advanceTimersByTimeAsync(1_000);
      expect(isWorkflowActuationRevokedError(await rejected)).toBe(true);
      expect(execute).toHaveBeenCalledTimes(1);
      expect(close).toHaveBeenCalledOnce();
    } finally {
      vi.useRealTimers();
    }
  });

  it("yields pending ownership contention even with a submission permit", async () => {
    const actuation = await admittedActuation();
    const journal = bindWorkflowActuationJournal({
      journal: new MemoryFraudProofWorkflowJournalStore(),
      permit: actuation.actuationPermit,
      decisionDigest: actuation.decisionDigest,
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash: actuation.headerHash,
    });
    const result = {
      kind: "pending",
      resumeOnObservation: true,
      reason: "funding is reserved",
    };
    const execute = vi.fn(async () => result);
    await expect(
      continuePendingWorkflow({
        invocation: {
          mode: "resume",
          deploymentFingerprint: DEPLOYMENT,
          category: "doubleSpend",
          headerHash: actuation.headerHash,
        },
        journal,
        execute,
      }),
    ).resolves.toBe(result);
    expect(execute).toHaveBeenCalledExactlyOnceWith("resume");
  });

  it("treats only null funding reobservation as temporary contention", async () => {
    const reobserve = vi.fn<() => Promise<unknown>>(async () => null);
    const funding = await runtimeFunding("step-one", { reobserve });
    const selected = unsafeWorkflowFundingReservationSelectedOutRefsForTest(
      funding.permit,
    );
    await expect(
      reobserveWorkflowFundingReservationTransaction({
        journal: funding.journal,
        transactionHash: "aa".repeat(32),
      }),
    ).resolves.toBe(false);
    expect(
      unsafeWorkflowFundingReservationSelectedOutRefsForTest(funding.permit),
    ).toEqual(selected);
    reobserve.mockResolvedValue(undefined);
    await expect(
      reobserveWorkflowFundingReservationTransaction({
        journal: funding.journal,
        transactionHash: "aa".repeat(32),
      }),
    ).rejects.toThrow();
    const failure = new Error("malformed durable identity");
    reobserve.mockRejectedValue(failure);
    await expect(
      reobserveWorkflowFundingReservationTransaction({
        journal: funding.journal,
        transactionHash: "aa".repeat(32),
      }),
    ).rejects.toBe(failure);
  });

  it.each([false, true])(
    "authorizes stale idle refresh only after every signed descendant resolves, and reobservation revokes it (read-only: %s)",
    async (readOnly) => {
      const journal = new MemoryFraudProofWorkflowJournalStore();
      const refreshIdle = vi.fn(
        async (): Promise<WorkflowFundingReservationSnapshot> =>
          funding.snapshot,
      );
      const funding = await runtimeFunding("step-one", {
        journal,
        refreshIdle,
      });
      const checkStaleRefresh = async (allowed: boolean) => {
        if (readOnly) return;
        funding.resolveInputs.mockResolvedValueOnce([]);
        await funding.begin();
        expect(refreshIdle).toHaveBeenLastCalledWith({
          expectedRevision: funding.snapshot.revision,
          releaseStaleInputs: allowed,
        });
      };
      const identity: FraudProofWorkflowIdentity = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        deploymentFingerprint: DEPLOYMENT,
        category: "doubleSpend",
        decisionDigest: funding.actuation.decisionDigest,
        target: {
          kind: "state_queue_header",
          headerHash: funding.actuation.headerHash,
        },
      };
      const workflowId = computeFraudProofWorkflowId(identity);
      const append = async (event: FraudProofWorkflowJournalEvent) => {
        const sequence = (await journal.load(workflowId)).length;
        await journal.append(
          {
            schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
            workflowId,
            identity,
            sequence,
            recordedAt: new Date().toISOString(),
            event,
          },
          sequence,
        );
      };
      await append({ kind: "started" });
      await append({
        kind: "prepared",
        artifact: {},
        artifactDigest: journalJsonDigest({}),
      });
      const parentHash = "a1".repeat(32),
        childHash = "a2".repeat(32);
      for (const [actionId, txHash] of [
        ["parent", parentHash],
        ["child", childHash],
      ] as const) {
        await append({
          kind: "preflight_passed",
          actionId,
          txHash,
          localEvaluator: "test",
          referenceScripts: [],
        });
        await append({
          kind: "submission_intent",
          actionId,
          txHash,
          attempt: 1,
          actionInput: { actionKind: "step-one" },
        });
        await append({
          kind: "reconciled",
          actionId,
          txHash,
          outcome: "confirmed",
        });
        await append({ kind: "confirmed", actionId, txHash });
      }
      await append({
        kind: "reobserved",
        actionId: "child",
        txHash: childHash,
      });
      await append({
        kind: "reobserved",
        actionId: "parent",
        txHash: parentHash,
      });
      await append({
        kind: "reconciled",
        actionId: "parent",
        txHash: parentHash,
        outcome: "not_found",
      });
      if (readOnly)
        funding.actuation.restrictToReconciliation(
          "parent recovery owns execution slot",
        );
      await releaseIdleWorkflowFundingReservation({ journal, workflowId });
      expect(funding.releaseIdle).not.toHaveBeenCalled();
      await checkStaleRefresh(false);
      await append({
        kind: "reobserved",
        actionId: "child",
        txHash: childHash,
      });
      await append({
        kind: "reconciled",
        actionId: "child",
        txHash: childHash,
        outcome: "confirmed",
      });
      await append({ kind: "confirmed", actionId: "child", txHash: childHash });
      await releaseIdleWorkflowFundingReservation({ journal, workflowId });
      expect(funding.releaseIdle).toHaveBeenCalledTimes(readOnly ? 1 : 0);
      await checkStaleRefresh(true);
      await append({
        kind: "reobserved",
        actionId: "child",
        txHash: childHash,
      });
      await releaseIdleWorkflowFundingReservation({ journal, workflowId });
      expect(funding.releaseIdle).toHaveBeenCalledTimes(readOnly ? 1 : 0);
      await checkStaleRefresh(false);
    },
  );

  it("returns a pending reconciliation without occupying the execution slot", async () => {
    const actuation = await admittedActuation();
    const journal = bindWorkflowActuationJournal({
      journal: new MemoryFraudProofWorkflowJournalStore(),
      permit: actuation.actuationPermit,
      decisionDigest: actuation.decisionDigest,
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash: actuation.headerHash,
    });
    actuation.restrictToReconciliation("target removed from canonical queue");
    const result = { kind: "pending", reason: "terminal inclusion absent" };
    const execute = vi.fn(async () => result);
    await expect(
      continuePendingWorkflow({
        invocation: {
          mode: "resume",
          deploymentFingerprint: DEPLOYMENT,
          category: "doubleSpend",
          headerHash: actuation.headerHash,
        },
        journal,
        execute,
      }),
    ).resolves.toBe(result);
    expect(execute).toHaveBeenCalledExactlyOnceWith("resume");
  });

  it("returns every non-pending continuation result unchanged", async () => {
    const actuation = await admittedActuation();
    const journal = bindWorkflowActuationJournal({
      journal: new MemoryFraudProofWorkflowJournalStore(),
      permit: actuation.actuationPermit,
      decisionDigest: actuation.decisionDigest,
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash: actuation.headerHash,
    });
    const values = [
      { kind: "terminal_included" },
      { kind: "stalled" },
      { kind: "awaiting_counterparty" },
      { kind: "unknown-result" },
      { state: "pending" },
      undefined,
      null,
      "pending",
    ];
    for (const value of values) {
      const execute = vi.fn(async () => value);
      await expect(
        continuePendingWorkflow({
          invocation: {
            mode: "resume",
            deploymentFingerprint: DEPLOYMENT,
            category: "doubleSpend",
            headerHash: actuation.headerHash,
          },
          journal,
          execute,
        }),
      ).resolves.toBe(value);
      expect(execute).toHaveBeenCalledExactlyOnceWith("resume");
    }
  });

  it("rejects a revoked decision permit before loading runtime infrastructure", async () => {
    const actuation = await admittedActuation();
    actuation.revoke("canonical rollback observed");
    const loadRuntime = vi.fn(async () => {
      throw new Error("revoked runner must not load infrastructure");
    });
    const runner = createManifestBoundWorkflowRunner({
      record: doubleSpendRecord({
        bindConfig: () => undefined,
        constructWorkflow: async (): Promise<DoubleSpendIdentity> => {
          throw new Error("revoked runner must not construct a workflow");
        },
        execute: async () => {
          throw new Error("revoked runner must not execute");
        },
      }),
      loadRuntime,
    });
    let rejected: unknown;
    try {
      await runner.runOrResume({
        mode: "resume",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        fundingReservationPermit: actuation.fundingReservationPermit,
        journalDirectory: "/tmp/midgard-runtime-revoked",
        runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
      });
    } catch (error) {
      rejected = error;
    }
    expect(rejected).toBeInstanceOf(WorkflowActuationRevokedError);
    expect(isWorkflowActuationRevokedError(rejected)).toBe(true);
    expect(
      isWorkflowActuationRevokedError(
        new WorkflowActuationRevokedError({
          decisionDigest: actuation.decisionDigest,
          rollbackGeneration: "7",
          checkpoint: "runner_start",
          revocationReason: "forged",
        }),
      ),
    ).toBe(false);
    expect(loadRuntime).not.toHaveBeenCalled();
  });

  it("checks the live permit at every shared workflow actuation boundary", async () => {
    const actuation = await admittedActuation();
    const journal = bindWorkflowActuationJournal({
      journal: new MemoryFraudProofWorkflowJournalStore(),
      permit: actuation.actuationPermit,
      decisionDigest: actuation.decisionDigest,
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash: actuation.headerHash,
    });
    expect(workflowActuationDecisionDigest(journal)).toBe(
      actuation.decisionDigest,
    );
    const checkpoints: readonly WorkflowActuationCheckpoint[] = [
      "workflow_resume",
      "before_observe",
      "before_preflight",
      "before_submit",
      "before_reconcile",
      "before_terminal_verify",
    ];
    for (const checkpoint of checkpoints) {
      expect(() =>
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint: DEPLOYMENT,
          category: "doubleSpend",
          headerHash: actuation.headerHash,
          checkpoint,
        }),
      ).not.toThrow();
    }
    actuation.revoke("canonical rollback observed");
    for (const checkpoint of checkpoints) {
      let rejected: unknown;
      try {
        assertWorkflowJournalActuation({
          journal,
          deploymentFingerprint: DEPLOYMENT,
          category: "doubleSpend",
          headerHash: actuation.headerHash,
          checkpoint,
        });
      } catch (error) {
        rejected = error;
      }
      expect(isWorkflowActuationRevokedError(rejected)).toBe(true);
      expect(rejected).toMatchObject({
        decisionDigest: actuation.decisionDigest,
        rollbackGeneration: "7",
        checkpoint,
      });
    }
  });

  it("exempts a family's requirements only under real reconciliation authority", async () => {
    const actuation = await admittedActuation();
    const record = defineFamilyApplication({
      category: "doubleSpend" as const,
      roster: {},
      requires: ["replayContext"] as const,
      bindConfig: () => undefined,
      constructWorkflow: async () => ({
        binding: {
          deploymentFingerprint: DEPLOYMENT,
          definition: {
            category: "doubleSpend" as const,
            headerHash: actuation.headerHash,
          },
        },
      }),
      execute: async () => ({}),
      bindsDecisionDigest: false,
    });
    const apply = () =>
      applyFamilyApplicationRecord({
        record,
        infrastructure: {
          manifest: {},
          blueprintJson: "{}",
          deploymentInfo: {},
          headerHash: actuation.headerHash,
          lucid: {} as never,
          signer: {} as never,
          source: {} as never,
          stateQueueMutationLeaseCoordinator: {} as never,
        },
        resolveReferenceScript: async () => {
          throw new Error("empty roster resolves nothing");
        },
        invocation: {
          deploymentFingerprint: DEPLOYMENT,
          category: "doubleSpend",
          headerHash: actuation.headerHash,
          reconciliationAuthority: actuation.actuationPermit,
        },
      });
    await expect(apply()).rejects.toThrow(
      "doubleSpend application claimed the reconciliation exemption under a permit that still admits actuation",
    );
    actuation.restrictToReconciliation("runtime test");
    await expect(apply()).resolves.toMatchObject({
      referenceScriptOutRefs: {},
    });
  });

  /**
   * One refusal test per check of the generic run-or-resume body. Every row of
   * the runner table is this body applied to a registry record, so each check
   * is tested here once instead of once per family. The workflow under test is
   * built through the public generic constructor, whose body is the same
   * function the admitted rows run.
   */
  describe("generic run-or-resume refusals", () => {
    type Actuation = Awaited<ReturnType<typeof admittedActuation>>;
    const identityOf = (
      actuation: Actuation,
      deploymentFingerprint: string = DEPLOYMENT,
    ) => ({
      binding: {
        deploymentFingerprint,
        definition: {
          category: "doubleSpend" as const,
          headerHash: actuation.headerHash,
        },
      },
    });
    const invocationOf = (actuation: Actuation, journalDirectory: string) => ({
      mode: "run" as const,
      category: "doubleSpend" as const,
      deploymentFingerprint: DEPLOYMENT,
      headerHash: actuation.headerHash,
      decisionDigest: actuation.decisionDigest,
      actuationPermit: actuation.actuationPermit,
      fundingReservationPermit: actuation.fundingReservationPermit,
      journalDirectory,
      runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
    });
    const driveGenericRunOrResume = async ({
      loaded,
      deploymentFingerprint,
      category,
      record = {},
      restrictToReconciliation = false,
    }: {
      readonly loaded: (
        actuation: Actuation,
        close: () => Promise<void>,
      ) => LoadedWorkflowRuntime;
      readonly deploymentFingerprint?: string;
      readonly category?: FraudProofCatalogueCategoryName;
      readonly record?: Partial<
        Pick<
          Parameters<typeof doubleSpendRecord>[0],
          "roster" | "requires" | "constructWorkflow"
        >
      >;
      readonly restrictToReconciliation?: boolean;
    }) => {
      const actuation = await admittedActuation();
      if (restrictToReconciliation)
        actuation.restrictToReconciliation("runtime refusal test");
      const directory = await mkdtemp(
        join(tmpdir(), "midgard-runtime-refusal-"),
      );
      const execute = vi.fn(async () => ({ kind: "unexpected" }));
      const close = vi.fn(async () => undefined);
      const loadRuntime = vi.fn(async () => loaded(actuation, close));
      const runner = createManifestBoundWorkflowRunner({
        record: doubleSpendRecord({
          bindConfig: () => undefined,
          constructWorkflow: async () =>
            identityOf(actuation, deploymentFingerprint),
          execute,
          ...record,
        }),
        loadRuntime,
      });
      const invocation = invocationOf(actuation, directory);
      const outcome = runner.runOrResume(
        category === undefined ? invocation : { ...invocation, category },
      );
      await outcome.catch(() => undefined);
      await rm(directory, { recursive: true, force: true });
      return { outcome, execute, close, loadRuntime };
    };
    const publicLoaded =
      (overrides: Partial<LoadedWorkflowRuntime> = {}) =>
      (
        actuation: Actuation,
        close: () => Promise<void>,
      ): LoadedWorkflowRuntime =>
        loadedRuntime(actuation, { close, ...overrides });

    it("refuses another category before loading any runtime configuration", async () => {
      const { outcome, loadRuntime, execute } = await driveGenericRunOrResume({
        loaded: publicLoaded(),
        category: "zeroInput",
      });
      await expect(outcome).rejects.toThrow(
        "production workflow runner category mismatch: expected=doubleSpend actual=zeroInput",
      );
      expect(loadRuntime).not.toHaveBeenCalled();
      expect(execute).not.toHaveBeenCalled();
    });

    it("refuses a loaded runtime that omits its transport disposer", async () => {
      const { outcome, execute } = await driveGenericRunOrResume({
        loaded: (actuation) => {
          const { close: _close, ...withoutDisposer } = publicLoaded()(
            actuation,
            async () => undefined,
          );
          return withoutDisposer as never;
        },
      });
      await expect(outcome).rejects.toThrow(
        "production workflow runtime config omitted its transport disposer",
      );
      expect(execute).not.toHaveBeenCalled();
    });

    it("refuses an unsupported runtime config schema and still disposes the transport", async () => {
      const { outcome, close, execute } = await driveGenericRunOrResume({
        loaded: publicLoaded({
          schemaVersion:
            "midgard-production-fraud-proof-runtime-config-v0" as never,
        }),
      });
      await expect(outcome).rejects.toThrow(
        "production workflow runtime config has an unsupported schema",
      );
      expect(close).toHaveBeenCalledOnce();
      expect(execute).not.toHaveBeenCalled();
    });

    it("refuses a retained-DA source that is not a public libp2p transport", async () => {
      const { outcome, close, execute } = await driveGenericRunOrResume({
        loaded: publicLoaded({
          retainedDaSources: [
            {
              sourceId: "operator-private-file",
              fetchPayloadByHeaderHash: async () => ({
                ok: false as const,
                sourceId: "operator-private-file",
                attempts: [],
              }),
            } as unknown as DaLibp2pRetainedDaSource,
          ],
        }),
      });
      await expect(outcome).rejects.toThrow(
        "production workflow runtime requires concrete public retained-DA libp2p sources",
      );
      expect(close).toHaveBeenCalledOnce();
      expect(execute).not.toHaveBeenCalled();
    });

    it("refuses a constructed workflow whose manifest identity differs from the invocation", async () => {
      const { outcome, close, execute } = await driveGenericRunOrResume({
        loaded: publicLoaded(),
        deploymentFingerprint: "ff".repeat(32),
      });
      await expect(outcome).rejects.toThrow(
        "manifest-bound workflow identity differs from the compiled CLI invocation",
      );
      expect(close).toHaveBeenCalledOnce();
      expect(execute).not.toHaveBeenCalled();
    });

    it("refuses loaded infrastructure that names another header or decision, and disposes it", async () => {
      for (const overrides of [
        { headerHash: "ee".repeat(32) },
        { decisionDigest: "ee".repeat(32) },
        { decisionDigest: undefined },
      ]) {
        const { outcome, close, execute } = await driveGenericRunOrResume({
          loaded: (actuation, close) =>
            loadedRuntime(actuation, {
              close,
              infrastructure: familyCommonInfrastructureForTest(
                actuation,
                overrides,
              ),
            }),
        });
        await expect(outcome).rejects.toThrow(
          "production workflow runtime infrastructure differs from the invocation",
        );
        expect(close).toHaveBeenCalledOnce();
        expect(execute).not.toHaveBeenCalled();
      }
    });

    it("applies the record inside the body: resolves its roster through the loaded resolver and refuses a missing requirement", async () => {
      const resolveReferenceScript = vi.fn(async () => {
        throw new Error("roster resolution reached the host resolver");
      });
      const missing = await driveGenericRunOrResume({
        loaded: (actuation, close) =>
          loadedRuntime(actuation, { close, resolveReferenceScript }),
        record: { requires: ["replayContext"] },
      });
      await expect(missing.outcome).rejects.toThrow(
        "doubleSpend application requires replayContext, which the host did not supply",
      );
      expect(resolveReferenceScript).not.toHaveBeenCalled();
      expect(missing.close).toHaveBeenCalledOnce();

      const rostered = await driveGenericRunOrResume({
        loaded: (actuation, close) =>
          loadedRuntime(actuation, { close, resolveReferenceScript }),
        record: { roster: { step01: "fraudProofDoubleSpend" } },
      });
      await expect(rostered.outcome).rejects.toThrow(
        "roster resolution reached the host resolver",
      );
      expect(resolveReferenceScript).toHaveBeenCalledWith({
        category: "doubleSpend",
        role: "step01",
        contractName: "fraudProofDoubleSpend",
      });
      expect(rostered.close).toHaveBeenCalledOnce();
      expect(rostered.execute).not.toHaveBeenCalled();
    });

    it("exempts the record's requirements under reconciliation-only authority and then demands the recovery surface", async () => {
      const { outcome, close, execute } = await driveGenericRunOrResume({
        loaded: publicLoaded(),
        record: { requires: ["replayContext"] },
        restrictToReconciliation: true,
      });
      await expect(outcome).rejects.toThrow(
        "manifest-bound workflow omitted its existing adapter recovery surface",
      );
      expect(close).toHaveBeenCalledOnce();
      expect(execute).not.toHaveBeenCalled();
    });

    it("checks the constructed decision digest against the invocation only when the record binds it", async () => {
      const directory = await mkdtemp(
        join(tmpdir(), "midgard-runtime-digest-"),
      );
      const execute = vi.fn(async () => ({ kind: "executed" }));
      const run = async (bindsDecisionDigest: boolean, index: number) => {
        const actuation = await admittedActuation();
        const runner = createFamilyApplicationWorkflowRunner(
          defineFamilyApplication({
            category: "doubleSpend" as const,
            roster: {},
            requires: [],
            bindConfig: () => undefined,
            constructWorkflow: async () => ({
              binding: {
                deploymentFingerprint: DEPLOYMENT,
                definition: {
                  category: "doubleSpend" as const,
                  headerHash: actuation.headerHash,
                },
              },
              decisionDigest: "ab".repeat(32),
            }),
            execute,
            bindsDecisionDigest,
          }),
          async () => loadedRuntime(actuation),
        );
        return await runner.runOrResume({
          mode: "run",
          category: "doubleSpend",
          deploymentFingerprint: DEPLOYMENT,
          headerHash: actuation.headerHash,
          decisionDigest: actuation.decisionDigest,
          actuationPermit: actuation.actuationPermit,
          fundingReservationPermit: actuation.fundingReservationPermit,
          journalDirectory: join(directory, `journal-${index}`),
          runtimeConfigPath: "/etc/midgard/fraud-proof-runtime-v1.json",
        });
      };
      await expect(run(true, 0)).rejects.toThrow(
        "doubleSpend manifest-bound workflow decision digest differs from invocation",
      );
      expect(execute).not.toHaveBeenCalled();
      await run(false, 1);
      expect(execute).toHaveBeenCalledOnce();
      await rm(directory, { recursive: true, force: true });
    });
  });
});
