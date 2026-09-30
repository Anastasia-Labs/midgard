import {
  decodeMidgardCekProgramMaterialEntry,
  encodeMidgardCekProgramMaterialEntry,
  type MidgardCekProgramMaterialEntry,
  midgardCekProgramMaterialKindTag,
} from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  Data,
  LucidEvolution,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { MidgardValidators, OutputReference } from "../common.js";
import { CekProgramMaterialDatum } from "../ledger-state.js";
import { UserEventBuildError } from "./internals.js";
import {
  type CekProgramMaterialPublication,
  deriveCekSinglePublication,
  minimumLovelaceForCekProgramMaterialPublication,
  minimumLovelaceForCekSinglePublication,
  type PublishCekProgramMaterialConfig,
  type PublishCekSinglePublicationConfig,
  resolveProtocolParameters,
} from "./tx-order.tx-order-material-carriage-vector.js";

export const deriveCekProgramMaterialPublications = (
  entries: readonly MidgardCekProgramMaterialEntry[],
): readonly CekProgramMaterialPublication[] => {
  if (entries.length === 0) {
    throw new Error("CEK program-material publication cannot be empty");
  }
  const seen = new Set<string>();
  return Object.freeze(
    entries.map((entry) => {
      const exact = decodeMidgardCekProgramMaterialEntry(
        encodeMidgardCekProgramMaterialEntry(entry),
      );
      const root = Buffer.from(exact.root).toString("hex");
      if (seen.has(root)) {
        throw new Error(`duplicate CEK program-material root ${root}`);
      }
      seen.add(root);
      const datum: CekProgramMaterialDatum = {
        kind: midgardCekProgramMaterialKindTag(exact.kind),
        root,
        preimage: exact.preimage.toString("hex"),
      };
      const datumCbor = Data.to(datum, CekProgramMaterialDatum);
      const datumBytes = Buffer.byteLength(datumCbor, "hex");
      if (datumBytes > MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes) {
        throw new Error(
          `CEK program-material datum ${root} exceeds the independently revealable L1 proof field bound`,
        );
      }
      return Object.freeze({ entry: exact, datum, datumCbor });
    }),
  );
};

export const buildUnsignedCekProgramMaterialProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekProgramMaterialConfig,
): Effect.Effect<TxSignBuilder, UserEventBuildError> =>
  Effect.tryPromise({
    try: async () => {
      const publications = deriveCekProgramMaterialPublications(config.entries);
      const protocolParameters = await resolveProtocolParameters(lucid);
      let tx = lucid.newTx();
      for (const publication of publications) {
        const minimumLovelace = minimumLovelaceForCekProgramMaterialPublication(
          {
            contracts,
            publication,
            coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
          },
        );
        tx = tx.pay.ToAddressWithData(
          contracts.cekProgramMaterial.spendingScriptAddress,
          { kind: "inline", value: publication.datumCbor },
          {
            lovelace:
              config.lovelacePerEntry === undefined
                ? minimumLovelace
                : config.lovelacePerEntry > minimumLovelace
                  ? config.lovelacePerEntry
                  : minimumLovelace,
          },
        );
      }
      return tx.complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: "Failed to publish V1 CEK program material",
        cause,
      }),
  });

/**
 * Publishes exactly one complete CEK graph as an immutable reference-only
 * inline datum. It has no spending path and therefore creates no mutable
 * state transition.
 */
export const buildUnsignedCekSinglePublicationProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekSinglePublicationConfig,
): Effect.Effect<TxSignBuilder, UserEventBuildError> =>
  Effect.tryPromise({
    try: async () => {
      const publication = deriveCekSinglePublication(config);
      const protocolParameters = await resolveProtocolParameters(lucid);
      const minimumLovelace = minimumLovelaceForCekSinglePublication({
        contracts,
        publication,
        coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
      });
      return lucid
        .newTx()
        .pay.ToAddressWithData(
          contracts.cekProgramMaterial.spendingScriptAddress,
          { kind: "inline", value: publication.datumCbor },
          {
            lovelace:
              config.lovelace === undefined
                ? minimumLovelace
                : config.lovelace > minimumLovelace
                  ? config.lovelace
                  : minimumLovelace,
          },
        )
        .complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: "Failed to publish V1 complete CEK program material",
        cause,
      }),
  });

export type TxOrderBuildMetadata = {
  readonly txOrderAddress: string;
  readonly txOrderId: OutputReference;
  readonly authNonceCbor: string;
  readonly txOrderAuthUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly validTo: number;
  readonly inclusionTime: number;
};

export const DEFAULT_TX_ORDER_LOVELACE = 3_000_000n;
