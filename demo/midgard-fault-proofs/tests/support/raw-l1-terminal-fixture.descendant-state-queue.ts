import {
  castStateQueueNodeToData,
  encodeLinkedListNodeView,
  hashBlockHeader,
  type Header,
  NO_DA_ATTESTATION,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
} from "@al-ft/midgard-sdk";
import { CML, toUnit } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { makeHeader } from "./emulator/header-fixtures.js";
import { hash32, output, raw } from "./raw-l1-terminal-fixture.output.js";

export const descendantStateQueueFixture = async ({
  header,
  headerHash,
  statePolicy,
  stateAddress,
  descendantOperatorCredential,
}: {
  readonly header: Header;
  readonly headerHash: string;
  readonly statePolicy: string;
  readonly stateAddress: string;
  readonly descendantOperatorCredential?: string;
}) => {
  const childHeader = {
    ...makeHeader(
      descendantOperatorCredential ?? header.operatorVkey,
      Number(header.endTime),
    ),
    prevHeaderHash: headerHash,
  };
  const childHash = await Effect.runPromise(hashBlockHeader(childHeader));
  return {
    childHash,
    burnChild: (mint: CML.Mint) =>
      mint.set(
        CML.ScriptHash.from_hex(statePolicy),
        CML.AssetName.from_hex(STATE_QUEUE_NODE_ASSET_NAME_PREFIX + childHash),
        -1n,
      ),
    child: raw(
      `${hash32("44")}#0`,
      output({
        address: stateAddress,
        assets: {
          lovelace: 3_000_000n,
          [toUnit(statePolicy, STATE_QUEUE_NODE_ASSET_NAME_PREFIX + childHash)]:
            1n,
        },
        datum: stateQueueNodeFixtureDatum(childHash, childHeader),
      }),
    ),
    continuedTargetDatum: stateQueueNodeFixtureDatum(headerHash, header),
  };
};

export const stateQueueNodeFixtureDatum = (
  key: string,
  header: Header,
  next?: string,
) =>
  encodeLinkedListNodeView({
    key: { Key: { key } },
    next: next === undefined ? "Empty" : { Key: { key: next } },
    data: castStateQueueNodeToData({
      proven_fraud: null,
      header,
      da_attestation: NO_DA_ATTESTATION,
    }) as never,
  });
