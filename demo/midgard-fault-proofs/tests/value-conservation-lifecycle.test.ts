import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { inspect } from "node:util";

import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxProofSource,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import {
  encodeMidgardForcedTxCanonical as forcedFullBytes,
  materializeMidgardForcedTxFromCanonical as forcedView,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, describe, expect, it } from "vitest";

import {
  certifyFaultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../src/field-opening.js";
import { submitRemoveFraudulentBlock } from "../src/index.js";
import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { publishProofChunks } from "../src/publish-proof-chunks.js";
import { commitCountedRoot } from "../src/transition-trace/phas.js";
import {
  buildEventToStepMembershipProof,
  buildForcedTransactionLeafMembershipProof,
  buildIndexedTraceProof,
} from "../src/transition-trace/witnesses.js";
import {
  admitValueConservationArtifact,
  VALUE_CONSERVATION_ARTIFACT,
  type ValueConservationArtifact,
} from "../src/value-not-preserved/artifact.js";
import { flattenMidgardValueAssets } from "../src/value-not-preserved/evidence.js";
import { createValueConservationFieldPrerequisite } from "../src/value-not-preserved/field-prerequisite.js";
import {
  conservationAcceptedSource,
  conservationForcedSource,
} from "../src/value-not-preserved/source-plan.js";
import {
  conservationStep,
  submitConservationAcceptedSource,
  submitConservationAction,
  submitConservationCancel,
} from "../src/value-not-preserved/submit-union.js";
import { submitValueNotPreservedInit } from "../src/value-not-preserved/submit-value-not-preserved-init.js";
import { planConservationFold } from "../src/value-not-preserved/union-plan.js";
import {
  ConservationClaim,
  ConservationTerminalArgs,
  ConservationUpdateArgs,
} from "../src/value-not-preserved/union-schemas.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../src/workflow/raw-l1-publication-observation.js";
import { buildDecodingBlockFixture } from "./support/native-script-decoding-emulator.js";
import { registerChunkedVerifyRewardAccount } from "./support/submit-init-emulator-shared.js";
import { realBlueprintPath } from "./support/submit-init-emulator-shared.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  buildRemovalDeploymentInfo,
  captureEmulatorSubmission,
  expectProofFit,
  funderPaymentKeyHash,
  network,
  publishPlainReferenceScriptUtxo,
  publishRemovalReferenceScripts,
  submitSetupTx,
} from "./support/submit-init-emulator-shared.js";
import { syntheticDeepMembershipProof } from "./support/synthetic-deep-proof.js";
import {
  buildValueNotPreservedFixture,
  makeValueNotPreservedEmulatorHarness,
  vnpOutput,
  vnpOutRef,
  vnpValue,
} from "./support/value-not-preserved-emulator.js";

const measurements: VanRossemFitMeasurement[] = [];
afterAll(async () => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
  await writeVanRossemFitLedger(
    fileURLToPath(
      new URL(
        "../../../docs/fault-proofs/size-plans/value-not-preserved-fit-ledger.json",
        import.meta.url,
      ),
    ),
    buildVanRossemFitLedger({
      category: "valueNotPreserved:00000019:testnet",
      blueprintSha256: createHash("sha256")
        .update(await readFile(realBlueprintPath))
        .digest("hex"),
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements,
    }),
  );
});

describe("universal value conservation", () => {
  it.each([
    {
      forced: true,
      maximum: false,
      cancellation: false,
      assetMaximum: false,
      proofMaximum: false,
      assetMode: "tokens",
      honest: false,
    },
    {
      forced: false,
      maximum: false,
      cancellation: false,
      assetMaximum: false,
      proofMaximum: false,
      assetMode: "tokens",
      honest: false,
    },
    {
      forced: true,
      maximum: true,
      cancellation: false,
      assetMaximum: false,
      proofMaximum: false,
      assetMode: "tokens",
      honest: false,
    },
    {
      forced: false,
      maximum: true,
      cancellation: false,
      assetMaximum: false,
      proofMaximum: false,
      assetMode: "tokens",
      honest: false,
    },
    {
      forced: true,
      maximum: false,
      cancellation: true,
      assetMaximum: false,
      proofMaximum: false,
      assetMode: "tokens",
      honest: false,
    },
    {
      forced: false,
      maximum: false,
      cancellation: true,
      assetMaximum: false,
      proofMaximum: false,
      assetMode: "tokens",
      honest: false,
    },
    {
      forced: true,
      maximum: false,
      cancellation: false,
      assetMaximum: true,
      proofMaximum: false,
      assetMode: "tokens",
      honest: false,
    },
    {
      forced: false,
      maximum: false,
      cancellation: false,
      assetMaximum: true,
      proofMaximum: false,
      assetMode: "tokens",
      honest: false,
    },
    {
      forced: true,
      maximum: false,
      cancellation: false,
      assetMaximum: false,
      proofMaximum: true,
      assetMode: "tokens",
      honest: false,
    },
    {
      forced: false,
      maximum: false,
      cancellation: false,
      assetMaximum: false,
      proofMaximum: true,
      assetMode: "tokens",
      honest: false,
    },
    ...[true, false].flatMap((forced) =>
      ["ada", "multi"].map((assetMode) => ({
        forced,
        maximum: false,
        cancellation: false,
        assetMaximum: false,
        proofMaximum: false,
        assetMode,
        honest: false,
      })),
    ),
    ...["ada", "tokens"].map((assetMode) => ({
      forced: true,
      maximum: false,
      cancellation: false,
      assetMaximum: false,
      proofMaximum: false,
      assetMode,
      honest: true,
    })),
  ])(
    "proves value conservation $forced, maximum=$maximum, cancellation=$cancellation, assets=$assetMaximum, proofs=$proofMaximum, domain=$assetMode, honest=$honest across mint and burn",
    async ({
      forced,
      maximum,
      cancellation,
      assetMaximum,
      proofMaximum,
      assetMode,
      honest,
    }) => {
      const harness = await makeValueNotPreservedEmulatorHarness({
        alwaysFraudProofCatalogue: false,
      });
      const record = async <T>(
        label: string,
        operation: () => Promise<T>,
      ): Promise<T> => {
        const capture = await captureEmulatorSubmission(
          harness.emulator,
          async () => {
            try {
              return await operation();
            } catch (cause) {
              throw new Error(`${label}: ${inspect(cause, { depth: 10 })}`);
            }
          },
        );
        for (const measurement of capture.measurements) {
          expectProofFit({
            stage: label,
            measurement,
            maxTxExMem: harness.emulator.protocolParameters.maxTxExMem,
            maxTxExSteps: harness.emulator.protocolParameters.maxTxExSteps,
          });
          measurements.push({
            name: `${expect.getState().currentTestName}:${label}:${measurements.length}`,
            kind:
              measurement.executionMemory === 0n ? "publication" : "lifecycle",
            maximumShape: expect.getState().currentTestName!,
            signedBytes: measurement.completeSignedBytes,
            memoryUnits: measurement.executionMemory,
            cpuUnits: measurement.executionSteps,
          });
        }
        return capture.result;
      };
      const policy = "ab".repeat(28);
      const tokens = (a: bigint, b: bigint) => [
        { policyIdHex: policy, assetNameHex: "01", quantity: a },
        { policyIdHex: policy, assetNameHex: "02", quantity: b },
      ];
      let fixture = await buildValueNotPreservedFixture({
        spentInputs: [
          {
            input: vnpOutRef("11", 0),
            spentValue: vnpValue(5_000_000n, tokens(2n, 5n)),
          },
        ],
        outputs: [
          vnpOutput({
            value: vnpValue(
              4_000_000n,
              tokens(forced && !honest ? 3n : 4n, 3n),
            ),
          }),
        ],
        mintItems: [
          {
            policyId: Buffer.from(policy, "hex"),
            assets: [
              { assetName: Buffer.from("01", "hex"), quantity: 1n },
              { assetName: Buffer.from("02", "hex"), quantity: -2n },
            ],
          },
        ],
      });
      if (assetMode === "ada")
        fixture = await buildValueNotPreservedFixture({
          spentInputs: [
            { input: vnpOutRef("11", 0), spentValue: vnpValue(5_000_000n) },
          ],
          outputs: [
            vnpOutput({
              value: vnpValue(forced && !honest ? 4_000_000n : 4_000_001n),
            }),
          ],
        });
      if (assetMode === "multi") {
        let selected:
          | { policyIdHex: string; assetNameHex: string; quantity: bigint }[]
          | undefined;
        for (let count = 140; count < 160 && selected === undefined; count++)
          for (let lastNameBytes = 0; lastNameBytes <= 32; lastNameBytes++) {
            const assets = Array.from({ length: count }, (_, index) => ({
              policyIdHex: index.toString(16).padStart(56, "0"),
              assetNameHex:
                index === count - 1 ? "11".repeat(lastNameBytes) : "",
              quantity: 1n,
            }));
            const material = buildCanonicalMidgardLedgerOutputMaterial({
              outputIndex: 0,
              outputCbor: encodeMidgardTxOutput(
                vnpOutput({ value: vnpValue(4_000_000n, assets) }),
              ),
            });
            if (material.descriptor.cardanoValueSize === 5000) {
              selected = assets;
              break;
            }
          }
        if (selected === undefined)
          throw new Error("maximum multi-policy fixture absent");
        const inputTokens = selected.slice(1).map((asset, index, assets) => ({
          ...asset,
          quantity: index === assets.length - 1 ? 2n : 1n,
        }));
        const first = selected[0]!;
        const last = selected.at(-1)!;
        fixture = await buildValueNotPreservedFixture({
          spentInputs: [
            {
              input: vnpOutRef("11", 0),
              spentValue: vnpValue(5_000_000n, inputTokens),
            },
          ],
          outputs: [
            vnpOutput({
              value: vnpValue(
                4_000_000n,
                selected.map((asset, index) => ({
                  ...asset,
                  quantity: !forced && index === 0 ? 2n : 1n,
                })),
              ),
            }),
          ],
          mintItems: [
            {
              policyId: Buffer.from(first.policyIdHex, "hex"),
              assets: [
                {
                  assetName: Buffer.from(first.assetNameHex, "hex"),
                  quantity: 1n,
                },
              ],
            },
            {
              policyId: Buffer.from(last.policyIdHex, "hex"),
              assets: [
                {
                  assetName: Buffer.from(last.assetNameHex, "hex"),
                  quantity: -1n,
                },
              ],
            },
          ],
        });
        expect(
          buildCanonicalMidgardLedgerOutputMaterial({
            outputIndex: 0,
            outputCbor: encodeMidgardTxOutput(fixture.outputs[0]!),
          }).descriptor.cardanoValueSize,
        ).toBe(5000);
      }
      if (assetMaximum) {
        const many = Array.from({ length: 1304 }, (_, i) => ({
          policyIdHex: policy,
          assetNameHex: (i === 0
            ? Buffer.alloc(0)
            : i <= 256
              ? Buffer.from([i - 1])
              : Buffer.from([(i - 257) >> 8, (i - 257) & 255])
          ).toString("hex"),
          quantity: i === 1303 ? 256n : 1n,
        }));
        const outputTokens = many.map((asset) => ({
          ...asset,
          quantity:
            asset.quantity + (!forced && asset.assetNameHex === "01" ? 1n : 0n),
        }));
        fixture = await buildValueNotPreservedFixture({
          spentInputs: [
            {
              input: vnpOutRef("11", 0),
              spentValue: vnpValue(5_000_000n, many),
            },
          ],
          outputs: [vnpOutput({ value: vnpValue(4_000_000n, outputTokens) })],
        });
        expect(
          buildCanonicalMidgardLedgerOutputMaterial({
            outputIndex: 0,
            outputCbor: encodeMidgardTxOutput(fixture.outputs[0]!),
          }).descriptor.cardanoValueSize,
        ).toBe(5000);
      }
      if (maximum) {
        const base = fixture.nativeTx;
        const otherBytes = [
          base.body.spendInputsPreimageCbor,
          base.body.referenceInputsPreimageCbor,
          base.body.requiredObserversPreimageCbor,
          base.body.requiredSignersPreimageCbor,
          base.body.mintPreimageCbor,
          base.witnessSet.addrTxWitsPreimageCbor,
          base.witnessSet.scriptTxWitsPreimageCbor,
          base.witnessSet.redeemerTxWitsPreimageCbor,
        ].reduce((n, bytes) => n + bytes.length, 0);
        let selected: ReturnType<typeof vnpOutput> | undefined;
        for (
          let chunks = 490;
          chunks <= 496 && selected === undefined;
          chunks++
        ) {
          for (let tail = 0; tail <= 64 && selected === undefined; tail++) {
            for (let empty = 0; empty <= 2; empty++) {
              const cbor = Buffer.concat([
                Buffer.from([0x9f]),
                ...Array.from({ length: chunks }, () =>
                  encodeCbor(Buffer.alloc(64, 0x42)),
                ),
                encodeCbor(Buffer.alloc(tail, 0x43)),
                Buffer.alloc(empty, 0x40),
                Buffer.from([0xff]),
              ]);
              const output = {
                ...fixture.outputs[0]!,
                datum: { kind: "inline" as const, cbor },
              };
              if (
                encodeCbor([encodeMidgardTxOutput(output)]).length +
                  otherBytes ===
                32768
              ) {
                selected = output;
                break;
              }
            }
          }
        }
        if (selected === undefined)
          throw new Error("maximum aggregate fixture not found");
        fixture = await buildValueNotPreservedFixture({
          spentInputs: fixture.ledger.spentInputs,
          outputs: [selected],
          mintItems: fixture.mintItems,
        });
        expect(fixture.outputsPreimageCbor.length + otherBytes).toBe(32768);
      }
      const orderKey = { transactionId: "dd".repeat(32), outputIndex: 0n };
      let block = await buildDecodingBlockFixture({
        operatorVkey: await funderPaymentKeyHash(harness.funderLucid),
        startTime: BigInt(
          alignUnixTimeToEmulatorSlotBoundary(
            harness.funderLucid,
            harness.emulator.now() + 120_000,
          ) - 1,
        ),
        priorLedgerRoot: fixture.ledger.rootHex,
        subject: forced
          ? {
              kind: "forced",
              nativeTx: fixture.nativeTx,
              orderKey,
              verdict: { ForcedTxInvalid: { reason: "ValueNotPreserved" } },
            }
          : { kind: "normal", nativeTx: fixture.nativeTx },
      });
      const eventKey: SDK.EventKey = forced
        ? { ForcedTransactionEventKey: { tx_order_id: orderKey } }
        : { L2TransactionEventKey: { tx_id: block.nativeTxId } };
      let eventMembership = await buildEventToStepMembershipProof({
        reconstruction: block.reconstruction,
        eventKey,
      });
      let traceMembership = await buildIndexedTraceProof({
        reconstruction: block.reconstruction,
        stepIndex: eventMembership.value.step_index,
      });
      const fields = {
        0: fixture.spendInputsPreimageCbor.toString("hex"),
        2: fixture.outputsPreimageCbor.toString("hex"),
        5: fixture.nativeTx.body.mintPreimageCbor.toString("hex"),
      };
      const spentInputs = await Promise.all(
        fixture.ledger.spentInputs.map(async (spent) => ({
          descriptorCbor: spent.descriptorCbor,
          proof: Data.from(
            (
              await fixture.ledger.trie.prove(
                Buffer.from(SDK.encodeMidgardTxInputCanonical(spent.input)),
              )
            ).toString("hex"),
            SDK.Proof,
          ),
          assets: flattenMidgardValueAssets(spent.spentValue),
        })),
      );
      let forcedMembership = forced
        ? await buildForcedTransactionLeafMembershipProof({
            reconstruction: block.reconstruction,
            eventKey,
          })
        : undefined;
      if (proofMaximum) {
        const ledger = syntheticDeepMembershipProof({
          key: Buffer.from(
            SDK.encodeMidgardTxInputCanonical(
              fixture.ledger.spentInputs[0]!.input,
            ),
          ),
          value: Buffer.from(spentInputs[0]!.descriptorCbor, "hex"),
          branchLevels: 64,
        });
        spentInputs[0] = {
          ...spentInputs[0]!,
          proof: Data.from(ledger.proofCbor, SDK.Proof),
        };
        traceMembership = {
          ...traceMembership,
          value: {
            ...traceMembership.value,
            pre_utxos_root: ledger.transactionsPhasRoot,
          },
        };
        const trace = syntheticDeepMembershipProof({
          key: Buffer.from(Data.to(traceMembership.key), "hex"),
          value: Buffer.from(
            Data.to(traceMembership.value, SDK.TransitionStep),
            "hex",
          ),
          branchLevels: 32,
        });
        const traceRoot = await commitCountedRoot({
          domain: traceMembership.domain,
          phasRoot: trace.transactionsPhasRoot,
          count: traceMembership.count,
        });
        traceMembership = {
          ...traceMembership,
          root: traceRoot,
          phas_root: trace.transactionsPhasRoot,
          proof: Data.from(trace.proofCbor, SDK.Proof),
        };
        const event = syntheticDeepMembershipProof({
          key: Buffer.from(Data.to(eventMembership.key, SDK.EventKey), "hex"),
          value: Buffer.from(
            Data.to(eventMembership.value, SDK.EventToStepValue),
            "hex",
          ),
          branchLevels: 32,
        });
        const eventRoot = await commitCountedRoot({
          domain: eventMembership.domain,
          phasRoot: event.transactionsPhasRoot,
          count: eventMembership.count,
        });
        eventMembership = {
          ...eventMembership,
          root: eventRoot,
          phas_root: event.transactionsPhasRoot,
          proof: Data.from(event.proofCbor, SDK.Proof),
        };
        block = {
          ...block,
          header: {
            ...block.header,
            transitionTraceRoot: traceRoot,
            eventToStepRoot: eventRoot,
          },
        };
        if (forcedMembership !== undefined) {
          const deep = syntheticDeepMembershipProof({
            key: Buffer.from(
              Data.to(forcedMembership.key, SDK.OutputReference),
              "hex",
            ),
            value: Buffer.from(
              Data.to(forcedMembership.value, SDK.ForcedInclusionTxV1),
              "hex",
            ),
            branchLevels: 64,
          });
          const root = await commitCountedRoot({
            domain: forcedMembership.domain,
            phasRoot: deep.transactionsPhasRoot,
            count: forcedMembership.count,
          });
          forcedMembership = {
            ...forcedMembership,
            root,
            phas_root: deep.transactionsPhasRoot,
            proof: Data.from(deep.proofCbor, SDK.Proof),
          };
          block = {
            ...block,
            header: { ...block.header, forcedTransactionsRoot: root },
          };
        } else {
          if (block.txInclusion == null)
            throw new Error("missing accepted inclusion");
          const deep = syntheticDeepMembershipProof({
            key: Buffer.from(block.nativeTxId, "hex"),
            value: Buffer.from(
              block.txInclusion.l2TransactionSourceCbor!,
              "hex",
            ),
            branchLevels: 64,
          });
          block = {
            ...block,
            header: {
              ...block.header,
              transactionsRoot: await commitCountedRoot({
                domain: SDK.ROOT_DOMAINS.transactionsV1,
                phasRoot: deep.transactionsPhasRoot,
                count: block.header.l2TransactionCount,
              }),
            },
            txInclusion: {
              ...block.txInclusion,
              transactionsPhasRoot: deep.transactionsPhasRoot,
              txMembershipProof: Data.from(deep.proofCbor, SDK.Proof),
              txMembershipProofCbor: deep.proofCbor,
            },
          };
        }
      }
      const source = forced
        ? conservationForcedSource({
            header: block.header,
            membership: forcedMembership!,
            transactionCbor: forcedFullBytes(
              forcedView(fixture.nativeTx),
            ).toString("hex"),
          })
        : {
            ...conservationAcceptedSource({
              header: block.header,
              transactionId: block.nativeTxId,
              fee: fixture.nativeTx.body.fee,
              claim: {
                AcceptedImbalance: {
                  asset:
                    assetMode === "ada"
                      ? "AdaAsset"
                      : {
                          TokenAsset: {
                            policy_id:
                              assetMode === "multi" ? "00".repeat(28) : policy,
                            asset_name: assetMode === "multi" ? "" : "01",
                          },
                        },
                  direction: "ClaimedAssetInflated",
                },
              },
            }),
            nativeTxCompactCbor: fixture.nativeTxCompactCbor,
          };
      const planningClaim: ConservationClaim = honest
        ? {
            AcceptedImbalance: {
              asset:
                assetMode === "ada"
                  ? "AdaAsset"
                  : { TokenAsset: { policy_id: policy, asset_name: "01" } },
              direction: "ClaimedAssetInflated",
            },
          }
        : source.source.claim;
      let plan = await planConservationFold({
        contracts: harness.family,
        source: { ...source.source, claim: planningClaim },
        eventMembership,
        traceMembership,
        nativeTxCompactCbor: source.nativeTxCompactCbor,
        fields,
        spentInputs,
      });
      if (honest) {
        const accepted = Data.to(planningClaim, ConservationClaim);
        const replaceClaim = (value: Data): Data => {
          if (Data.to(value) === accepted)
            return Data.from(Data.to("ForcedConservation", ConservationClaim));
          if (value instanceof Constr)
            return new Constr(value.index, value.fields.map(replaceClaim));
          if (Array.isArray(value)) return value.map(replaceClaim);
          if (value instanceof Map)
            return new Map(
              [...value].map(([key, child]) => [
                replaceClaim(key),
                replaceClaim(child),
              ]),
            );
          return value;
        };
        plan = {
          ...plan,
          actions: plan.actions.map((action) => ({
            ...action,
            inputState: Data.to(replaceClaim(Data.from(action.inputState))),
            outputState:
              action.outputState === null
                ? null
                : Data.to(replaceClaim(Data.from(action.outputState))),
            ...(action.position === "unionTerminal"
              ? {
                  args: Data.to(
                    {
                      ...Data.from(action.args, ConservationTerminalArgs),
                      witness: null,
                    },
                    ConservationTerminalArgs,
                  ),
                }
              : {}),
          })),
        };
      }
      expect(plan.finalBalance.lovelace_delta).toBe(
        (!forced || honest) && assetMode === "ada" ? -1n : 0n,
      );
      if ((forced && !honest) || assetMode === "ada")
        expect(plan.finalBalance.asset_delta_root).toBe("00".repeat(32));
      else expect(plan.finalBalance.asset_delta_root).not.toBe("00".repeat(32));
      const artifact: ValueConservationArtifact = {
        schemaVersion: VALUE_CONSERVATION_ARTIFACT,
        headerCbor: Data.to(block.header, SDK.Header),
        transactionCbor: (forced
          ? forcedFullBytes(forcedView(fixture.nativeTx))
          : encodeMidgardNativeTxCanonical(fixture.nativeTx)
        ).toString("hex"),
        claimCbor: Data.to(source.source.claim, ConservationClaim),
        forcedMembershipCbor:
          forcedMembership === undefined
            ? null
            : Data.to(
                forcedMembership,
                SDK.ForcedTransactionSourceMembershipProof,
              ),
        acceptedSourceCbor: block.txInclusion?.l2TransactionSourceCbor ?? null,
        acceptedPhasRoot: block.txInclusion?.transactionsPhasRoot ?? null,
        acceptedProofCbor:
          block.txInclusion === null
            ? null
            : Data.to(block.txInclusion.txMembershipProof, SDK.Proof),
        eventCbor: Data.to(eventMembership, SDK.EventToStepMembershipProof),
        transitionCbor: Data.to(traceMembership, SDK.IndexedTraceProof),
        inputs: fixture.ledger.spentInputs.map((spent, index) => ({
          outputCbor: spent.outputCbor,
          proofCbor: Data.to(spentInputs[index]!.proof, SDK.Proof),
        })),
      };
      if (honest)
        await expect(
          admitValueConservationArtifact(artifact, harness.family),
        ).rejects.toThrow("honest rejection");
      else {
        const admitted = await admitValueConservationArtifact(
          JSON.parse(JSON.stringify(artifact)),
          harness.family,
        );
        expect(admitted.actions).toStrictEqual([
          ...source.actions,
          ...plan.actions,
        ]);
        await expect(
          admitValueConservationArtifact(
            { ...artifact, inputs: [] },
            harness.family,
          ),
        ).rejects.toThrow("omitted spent input");
        await expect(
          admitValueConservationArtifact(
            {
              ...artifact,
              transitionCbor: Data.to(
                {
                  ...traceMembership,
                  value: {
                    ...traceMembership.value,
                    pre_utxos_root: "44".repeat(32),
                  },
                },
                SDK.IndexedTraceProof,
              ),
            },
            harness.family,
          ),
        ).rejects.toThrow("authenticated membership");
      }
      const setup = await submitSetupTx({
        lucid: harness.funderLucid,
        contracts: harness.contracts,
        nonceUtxo: harness.nonceUtxo,
        catalogue: harness.catalogue,
        header: block.header,
      });
      const actions = [...source.actions, ...plan.actions];
      const references = new Map<string, UTxO>();
      for (const action of actions) {
        if (references.has(action.position)) continue;
        const publication = await record(
          `reference:${action.position}`,
          async () =>
            publishPlainReferenceScriptUtxo({
              lucid: harness.proverLucid,
              script: conservationStep(harness.family, action.position)
                .spendingScript,
              label: `value conservation ${action.position}`,
            }),
        );
        references.set(action.position, publication.utxo);
      }
      const init = await record("initialize", async () =>
        submitValueNotPreservedInit({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          network,
          contracts: harness.family,
          category: harness.category,
          catalogue: {
            policyId: harness.contracts.fraudProofCatalogue.policyId,
            spendingScriptAddress:
              harness.contracts.fraudProofCatalogue.spendingScriptAddress,
            root: harness.catalogue.root,
          },
          signer: harness.proverSigner,
          fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
          witnessReferenceScripts: harness.witnessReferenceScripts,
        }),
      );
      let threadOutRef = init.nextThreadOutRef;
      const fieldsReady = new Map<
        0 | 2 | 5,
        {
          planned: ReturnType<typeof planFaultProofFieldOpening>;
          carriageUtxos: readonly UTxO[];
          certificateUtxo?: UTxO;
        }
      >();
      const certificateReference = maximum
        ? (
            await record("installed:certificate-reference", () =>
              publishPlainReferenceScriptUtxo({
                lucid: harness.proverLucid,
                script:
                  harness.contracts.fieldPreimageCertificate.mintingScript,
                label: "value conservation certificate",
              }),
            )
          ).utxo
        : undefined;
      for (const fieldIndex of [0, 2, 5] as const) {
        if (maximum) {
          if (certificateReference === undefined)
            throw new Error("missing certificate reference");
          const makePort = () =>
            createValueConservationFieldPrerequisite({
              contracts: harness.family,
              certificate: {
                policyId: harness.contracts.fieldPreimageCertificate.policyId,
                mintingScript:
                  harness.contracts.fieldPreimageCertificate.mintingScript,
                referenceScriptUtxo: certificateReference,
              },
              lucid: harness.proverLucid,
              network,
              signer: harness.proverSigner,
              transactionConfirmed: async () => true,
              publications: {
                observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
                observeExact: async (input) => {
                  const found = (
                    await harness.proverLucid.utxosAt(input.address)
                  ).find(
                    (utxo) =>
                      `${utxo.txHash}#${utxo.outputIndex}` ===
                        input.expectedOutRef &&
                      utxo.datum === input.expectedDatumCbor &&
                      (input.expectedUnit === undefined ||
                        utxo.assets[input.expectedUnit] === 1n),
                  );
                  return found === undefined
                    ? { kind: "not_found" }
                    : { kind: "confirmed", outRef: input.expectedOutRef };
                },
              },
            });
          const index = actions.findIndex(
            (action) => action.fieldIndex === fieldIndex,
          );
          const baseAction = {
            actionId: `fold:${index}`,
            input: { stage: "fold", index },
          };
          for (let attempt = 0; attempt < 16; attempt++) {
            const next = await makePort().inspect({
              headerHash: setup.headerHash,
              baseAction,
              artifact,
              entries: [],
            });
            if (next.kind === "satisfied" || next.kind === "not_required")
              break;
            if (next.kind !== "required")
              throw new Error("unexpected pending field prerequisite");
            const captured = await makePort().capture({
              headerHash: setup.headerHash,
              action: next.action,
              artifact,
            });
            const txHash = await record(
              `installed:field:${fieldIndex}:${attempt}`,
              () => captured.transaction.signed.submit(),
            );
            harness.emulator.awaitBlock(1);
            expect(
              (
                await makePort().reconcile({
                  headerHash: setup.headerHash,
                  action: next.action,
                  artifact,
                  txHash,
                  durableRecovery: JSON.parse(
                    JSON.stringify(captured.durableRecovery),
                  ),
                })
              ).kind,
            ).toBe("confirmed");
          }
          const resolved = await makePort().resolveAuthenticated({
            headerHash: setup.headerHash,
            action: baseAction,
            artifact,
          });
          const planned = planFaultProofFieldOpening({
            anchorSourceKind: forced ? 1n : 0n,
            fieldIndex,
            anchorTxId: source.source.transaction_id,
            nativeTxCompactCbor: source.nativeTxCompactCbor,
            itemCbors: decodeMidgardFieldPreimage(
              Buffer.from(fields[fieldIndex], "hex"),
            ),
            owner: harness.proverSigner.paymentKeyHash,
            publish: true,
            label: "value conservation",
          });
          if (planned.plan.tier === "Certified")
            expect(resolved.certificate).toBeDefined();
          fieldsReady.set(fieldIndex, {
            planned,
            carriageUtxos: resolved.publications,
            ...(resolved.certificate === undefined
              ? {}
              : { certificateUtxo: resolved.certificate }),
          });
          continue;
        }

        const planned = planFaultProofFieldOpening({
          anchorSourceKind: forced ? 1n : 0n,
          fieldIndex,
          anchorTxId: source.source.transaction_id,
          nativeTxCompactCbor: source.nativeTxCompactCbor,
          itemCbors: decodeMidgardFieldPreimage(
            Buffer.from(fields[fieldIndex], "hex"),
          ),
          owner: harness.proverSigner.paymentKeyHash,
          label: "value conservation",
        });
        const carriageUtxos =
          planned.plan.tier === "Inline"
            ? []
            : await record(`field:${fieldIndex}:publish`, async () =>
                publishFaultProofFieldCarriage({
                  lucid: harness.proverLucid,
                  signer: harness.proverSigner,
                  planned,
                  publisherAddress: harness.proverSigner.address,
                  label: "value conservation",
                }),
              );
        const certificateUtxo =
          planned.plan.tier === "Certified"
            ? (
                await record(`field:${fieldIndex}:certify`, async () =>
                  certifyFaultProofFieldCarriage({
                    lucid: harness.proverLucid,
                    network,
                    signer: harness.proverSigner,
                    planned,
                    certificatePolicyId:
                      harness.contracts.fieldPreimageCertificate.policyId,
                    certificateMintingScript:
                      harness.contracts.fieldPreimageCertificate.mintingScript,
                    certificateReferenceScriptUtxo: (
                      await publishPlainReferenceScriptUtxo({
                        lucid: harness.proverLucid,
                        script:
                          harness.contracts.fieldPreimageCertificate
                            .mintingScript,
                        label: "value conservation field certificate",
                      })
                    ).utxo,
                    chunkUtxos: carriageUtxos,
                    compactCbor: source.nativeTxCompactCbor,
                    witnessSetCompactCbor: deriveMidgardNativeTxProofSource(
                      fixture.nativeTx,
                    ).witnessSetCompactCbor.toString("hex"),
                  }),
                )
              ).certificateUtxo
            : undefined;
        fieldsReady.set(fieldIndex, {
          planned,
          carriageUtxos,
          ...(certificateUtxo === undefined ? {} : { certificateUtxo }),
        });
      }
      if (proofMaximum && !forced)
        await record("source:chunked-reward-registration", () =>
          registerChunkedVerifyRewardAccount(
            harness.proverLucid,
            harness.realBlueprint,
          ),
        );
      const removalReferences = await record("removal:references", async () =>
        publishRemovalReferenceScripts({
          lucid: harness.proverLucid,
          contracts: harness.contracts,
        }),
      );
      const publishedSourceProof =
        proofMaximum && !forced
          ? (
              await record("source:proof-publication", () =>
                publishProofChunks({
                  lucid: harness.proverLucid,
                  network,
                  signer: harness.proverSigner,
                  proofCbor: block.txInclusion!.txMembershipProofCbor!,
                }),
              )
            ).chunks
          : undefined;
      let checkedMutation = false;
      const cancelled = new Set<string>();
      for (let index = 0; index < actions.length; index++) {
        const action = actions[index]!;
        if (!maximum && !checkedMutation && action.position === "unionUpdate") {
          const args = Data.from(action.args, ConservationUpdateArgs);
          await expect(
            submitConservationAction({
              lucid: harness.proverLucid,
              contracts: harness.family,
              categoryId: harness.category.categoryId,
              headerHash: setup.headerHash,
              signer: harness.proverSigner,
              threadOutRef,
              action: {
                ...action,
                args: Data.to(
                  { ...args, old_delta: args.old_delta + 1n },
                  ConservationUpdateArgs,
                ),
              },
              referenceScriptUtxo: references.get(action.position)!,
              witnessReferenceScripts: harness.witnessReferenceScripts,
            }),
          ).rejects.toThrow();
          await expect(
            submitConservationAction({
              lucid: harness.proverLucid,
              contracts: harness.family,
              categoryId: harness.category.categoryId,
              headerHash: setup.headerHash,
              signer: harness.proverSigner,
              threadOutRef,
              action: { ...action, inputState: actions[0]!.args },
              referenceScriptUtxo: references.get(action.position)!,
              witnessReferenceScripts: harness.witnessReferenceScripts,
            }),
          ).rejects.toThrow("live checkpoint differs");
          checkedMutation = true;
        }

        if (cancellation && !cancelled.has(action.position)) {
          await record(`cancel:${action.position}`, async () =>
            submitConservationCancel({
              lucid: harness.proverLucid,
              contracts: harness.family,
              categoryId: harness.category.categoryId,
              signer: harness.proverSigner,
              threadOutRef,
              referenceScriptUtxo: references.get(action.position)!,
              witnessReferenceScripts: harness.witnessReferenceScripts,
            }),
          );
          cancelled.add(action.position);
          expect(
            (
              await harness.proverLucid.utxosAt(
                conservationStep(harness.family, action.position)
                  .spendingScriptAddress,
              )
            ).some((u) => u.assets[init.computationThreadUnit] === 1n),
          ).toBe(false);
          const restarted = await record(
            `restart:${action.position}`,
            async () =>
              submitValueNotPreservedInit({
                lucid: harness.proverLucid,
                blueprint: harness.realBlueprint,
                network,
                contracts: harness.family,
                category: harness.category,
                catalogue: {
                  policyId: harness.contracts.fraudProofCatalogue.policyId,
                  spendingScriptAddress:
                    harness.contracts.fraudProofCatalogue.spendingScriptAddress,
                  root: harness.catalogue.root,
                },
                signer: harness.proverSigner,
                fraudulentBlockOutRef: setup.fraudulentBlockOutRef,
                witnessReferenceScripts: harness.witnessReferenceScripts,
              }),
          );
          threadOutRef = restarted.nextThreadOutRef;
          index = -1;
          continue;
        }
        if (honest && action.position === "unionTerminal") {
          await expect(
            submitConservationAction({
              lucid: harness.proverLucid,
              contracts: harness.family,
              categoryId: harness.category.categoryId,
              headerHash: setup.headerHash,
              signer: harness.proverSigner,
              threadOutRef,
              action,
              referenceScriptUtxo: references.get(action.position)!,
              witnessReferenceScripts: harness.witnessReferenceScripts,
            }),
          ).rejects.toThrow();
          await record("honest:cancel-terminal", () =>
            submitConservationCancel({
              lucid: harness.proverLucid,
              contracts: harness.family,
              categoryId: harness.category.categoryId,
              signer: harness.proverSigner,
              threadOutRef,
              referenceScriptUtxo: references.get(action.position)!,
              witnessReferenceScripts: harness.witnessReferenceScripts,
            }),
          );
          expect(
            (
              await harness.proverLucid.utxosAt(
                harness.family.fraudProof.spendingScriptAddress,
              )
            ).some(
              (utxo) =>
                utxo.assets[
                  harness.family.fraudProof.policyId +
                    harness.category.categoryId +
                    setup.headerHash
                ] === 1n,
            ),
          ).toBe(false);
          return;
        }
        const result = await record(`${index}:${action.position}`, async () => {
          if (action.position === "unionAcceptedSource") {
            if (block.txInclusion == null)
              throw new Error("accepted source absent");
            const [transactionId, index] = threadOutRef.split("#");
            const [threadUtxo] = await harness.proverLucid.utxosByOutRef([
              { txHash: transactionId!, outputIndex: Number(index) },
            ]);
            if (threadUtxo === undefined) throw new Error("thread absent");
            return {
              kind: "advanced" as const,
              ...(await submitConservationAcceptedSource({
                lucid: harness.proverLucid,
                blueprint: harness.realBlueprint,
                network,
                contracts: harness.family,
                source: source.source,
                signer: harness.proverSigner,
                threadUtxo,
                threadToken: {
                  unit: init.computationThreadUnit,
                  fraudulentHeaderHash: setup.headerHash,
                },
                stateQueueBlockOutRef: setup.fraudulentBlockOutRef,
                txInclusion: block.txInclusion,
                publishedProofChunks: publishedSourceProof,
                referenceScriptUtxo: references.get(action.position)!,
                witnessReferenceScripts: harness.witnessReferenceScripts,
                awaitConfirmation: true,
              })),
            };
          }
          return await submitConservationAction({
            lucid: harness.proverLucid,
            contracts: harness.family,
            categoryId: harness.category.categoryId,
            headerHash: setup.headerHash,
            signer: harness.proverSigner,
            threadOutRef,
            action,
            referenceScriptUtxo: references.get(action.position)!,
            ...(action.fieldIndex === undefined
              ? {}
              : { field: fieldsReady.get(action.fieldIndex)! }),
            ...(action.position === "unionOutputScan" &&
            fieldsReady.get(2)!.carriageUtxos.length > 0
              ? { chunkUtxos: fieldsReady.get(2)!.carriageUtxos }
              : {}),
            witnessReferenceScripts: harness.witnessReferenceScripts,
          });
        });
        if (result.kind === "advanced") threadOutRef = result.nextThreadOutRef;
        else expect(index).toBe(actions.length - 1);
      }
      const removal = await record("remove", async () =>
        submitRemoveFraudulentBlock({
          lucid: harness.proverLucid,
          blueprint: harness.realBlueprint,
          deploymentInfo: buildRemovalDeploymentInfo(
            harness.contracts,
            harness.catalogue,
            { removalReferenceScripts: removalReferences.published },
          ),
          network,
          signer: harness.proverSigner,
          fraudCategory: "valueNotPreserved",
          fraudulentHeaderHash: setup.headerHash,
          awaitConfirmation: true,
          requireReferenceScripts: true,
          validFrom: BigInt(harness.emulator.now() - 120_000),
          validTo: BigInt(harness.emulator.now() + 300_000),
        }),
      );
      expect(removal.transactions).toHaveLength(1);
      const unit =
        harness.family.fraudProof.policyId +
        harness.category.categoryId +
        setup.headerHash;
      expect(
        (
          await harness.proverLucid.utxosAt(
            harness.family.fraudProof.spendingScriptAddress,
          )
        ).some((u) => u.assets[unit] === 1n),
      ).toBe(true);
    },
    1_800_000,
  );
});
