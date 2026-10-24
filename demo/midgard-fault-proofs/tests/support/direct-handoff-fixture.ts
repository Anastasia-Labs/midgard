import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  generateEmulatorAccount,
  getAddressDetails,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { prepareDirectScriptMaterial } from "../../src/direct-script-derivation/index.js";
import { reconstructDaPayload } from "../../src/transition-trace/reconstruct.js";
import {
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
} from "../helpers/canonical-block-evidence-fixture.js";
import { directSourceBatchFixture } from "./direct-source-fixture.js";
import { network } from "./submit-init-emulator-shared.js";

const emptyOrReceiveMaterial = async (empty: boolean) => {
  const outputs = empty
    ? []
    : [0x22, 0x33].map((byte) =>
        encodeMidgardTxOutput({
          address: Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, byte)]),
          value: { lovelace: 5_000_000n, assets: new Map() },
        }),
      );
  const tx = buildFixtureTransaction({ spendInputs: [], outputs, fee: 0n });
  const eventKey: SDK.EventKey = { L2TransactionEventKey: { tx_id: tx.txId } };
  const fixture = await buildCanonicalBlockFixture({
    transactions: [tx],
    transitionSteps: [
      {
        schema_version: 1n,
        step_index: 0n,
        event_key: eventKey,
        phase: "L2Transaction",
        pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
        post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
      },
    ],
  });
  return prepareDirectScriptMaterial({
    current: await reconstructDaPayload({
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
    }),
    eventKey,
  });
};

export const directHandoffMaterial = async (
  family: "input" | "witness" | "receive",
  empty: boolean,
) =>
  family !== "receive" && !empty
    ? (await directSourceBatchFixture()).material
    : emptyOrReceiveMaterial(empty);

export const directHandoffContext = async (blueprint: unknown) => {
  const owner = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const stranger = generateEmulatorAccount({ lovelace: 40_000_000_000n });
  const prover = getAddressDetails(owner.address).paymentCredential!.hash;
  const strangerHash = getAddressDetails(stranger.address).paymentCredential!
    .hash;
  const policy = "cc".repeat(28),
    nextHash = "ee".repeat(28),
    certificate = "77".repeat(28);
  const receive = await Effect.runPromise(
    SDK.buildDirectReceiveContracts({
      blueprint,
      network,
      computationThreadPolicyId: policy,
      fieldPreimageCertificatePolicyId: certificate,
      nextStepScriptHash: nextHash,
    }),
  );
  const source = await Effect.runPromise(
    SDK.buildDirectSourceContracts({
      blueprint,
      network,
      computationThreadPolicyId: policy,
      fieldPreimageCertificatePolicyId: certificate,
      nextStepScriptHash: receive.start.spendingScriptHash,
    }),
  );
  return {
    owner,
    stranger,
    prover,
    strangerHash,
    source,
    receive,
    policy,
    nextHash,
    certificate,
  };
};

/** The seeded component-prefix thread each handoff family starts from. */
export const directHandoffInitialThread = ({
  family,
  material,
  prover,
  source,
  receive,
  sourceState,
}: {
  readonly family: "input" | "witness" | "receive";
  readonly material: Awaited<ReturnType<typeof directHandoffMaterial>>;
  readonly prover: string;
  readonly source: Awaited<ReturnType<typeof directHandoffContext>>["source"];
  readonly receive: Awaited<ReturnType<typeof directHandoffContext>>["receive"];
  readonly sourceState: SDK.DirectSourceScanState;
}) => {
  const initial =
    family === "input"
      ? source.start
      : family === "witness"
        ? source.grammar
        : receive.start;
  const initialDatum =
    family === "input"
      ? Data.to(
          { fraud_prover: prover, data: material.bound },
          SDK.DirectScriptBoundDatum,
        )
      : family === "witness"
        ? Data.to(
            { fraud_prover: prover, data: sourceState },
            SDK.DirectSourceScanDatum,
          )
        : Data.to(
            {
              fraud_prover: prover,
              data: {
                bound: material.bound,
                source_index: sourceState.source_index,
              },
            },
            SDK.DirectReceiveStartDatum,
          );
  return { initial, initialDatum };
};
