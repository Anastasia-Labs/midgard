import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

/** Durable preimages. Authority comes from the commitment in the L1 checkpoint. */
export type TransitionDepositOpening = Readonly<{
  commitmentCbor: string;
  openingCbor: string;
}>;

export const transitionDepositOpening = (
  captured: Pick<
    ReturnType<typeof SDK.captureEventHistoryWitness>,
    "commitment" | "openingCbor"
  >,
): TransitionDepositOpening => ({
  commitmentCbor: Data.to(captured.commitment, SDK.EventHistoryCommitment),
  openingCbor: captured.openingCbor,
});

export const reopenTransitionDeposit = (
  retained: TransitionDepositOpening,
  policy: string,
  source: {
    key: SDK.OutputReference;
    value: SDK.DepositInfo;
    valueCbor?: string;
  },
  checkpointCommitmentCbor?: string,
) => {
  const commitment = Data.from(
    retained.commitmentCbor,
    SDK.EventHistoryCommitment,
  );
  const opening = Data.from(retained.openingCbor, SDK.EventHistoryOpening);
  if (
    commitment.kind !== "Deposit" ||
    commitment.policy !== policy ||
    (checkpointCommitmentCbor !== undefined &&
      Data.to(
        Data.from(checkpointCommitmentCbor, SDK.EventHistoryCommitment),
        SDK.EventHistoryCommitment,
      ) !== Data.to(commitment, SDK.EventHistoryCommitment)) ||
    !SDK.opensEventHistoryCommitmentCbor(
      commitment,
      plutusConstrFieldCbor(retained.openingCbor, [0]),
      plutusConstrFieldCbor(retained.openingCbor, [1]),
    ) ||
    !("DepositPayload" in opening.payload)
  )
    throw new Error(
      "Transition deposit opening differs from its authenticated commitment",
    );
  const event = opening.payload.DepositPayload.event;
  const infoCbor = plutusConstrFieldCbor(retained.openingCbor, [0, 0, 1]);
  if (
    source.valueCbor !== undefined &&
    Data.to(Data.from(source.valueCbor, SDK.DepositInfo), SDK.DepositInfo) !==
      Data.to(source.value, SDK.DepositInfo)
  )
    throw new Error(
      "Transition deposit source view differs from its raw bytes",
    );
  if (
    Data.to(event.id, SDK.OutputReference) !==
      Data.to(source.key, SDK.OutputReference) ||
    aikenSerialisedPlutusDataCborPreservingMapOrder(infoCbor) !==
      aikenSerialisedPlutusDataCborPreservingMapOrder(
        source.valueCbor ?? Data.to(source.value, SDK.DepositInfo),
      )
  )
    throw new Error(
      "Transition deposit source differs from the authenticated event",
    );
  return { commitment, opening, event, infoCbor };
};
