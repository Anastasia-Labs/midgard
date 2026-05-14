import { Data } from "@lucid-evolution/lucid";
import {
  OutputReference,
  hashHexWithBlake2b256,
  type OutputReference as OutputReferenceData,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { parseHex, parseInteger } from "./submit-step-01.js";
import { aikenSerialisedPlutusDataCbor } from "./plutus-data-cbor.js";

const outputReferencesSchema = Data.Array(OutputReference as never);

export const encodeOutputReferences = (
  outputReferences: readonly OutputReferenceData[],
): string =>
  Data.to([...outputReferences] as never, outputReferencesSchema as never);

export const parseOutputReferences = (
  value: unknown,
  label: string,
): readonly OutputReferenceData[] => {
  if (!Array.isArray(value)) {
    throw new Error(`${label} must be a JSON array.`);
  }
  const inputs = value.map((entry, index): OutputReferenceData => {
    if (typeof entry !== "object" || entry === null || Array.isArray(entry)) {
      throw new Error(`${label}[${index.toString()}] must be an object.`);
    }
    const record = entry as Record<string, unknown>;
    return {
      transactionId: parseHex(
        record.transactionId,
        `${label}[${index.toString()}].transactionId`,
        32,
      ),
      outputIndex: parseInteger(
        record.outputIndex,
        `${label}[${index.toString()}].outputIndex`,
      ),
    };
  });
  return Data.from(
    encodeOutputReferences(inputs),
    outputReferencesSchema as never,
  ) as unknown as readonly OutputReferenceData[];
};

export const hashOutputReferences = async (
  outputReferences: readonly OutputReferenceData[],
): Promise<string> =>
  await Effect.runPromise(
    hashHexWithBlake2b256(
      aikenSerialisedPlutusDataCbor(
        encodeOutputReferences(outputReferences),
      ),
    ),
  );

export const sameOutputReference = (
  left: OutputReferenceData,
  right: OutputReferenceData,
): boolean =>
  left.transactionId === right.transactionId &&
  left.outputIndex === right.outputIndex;
