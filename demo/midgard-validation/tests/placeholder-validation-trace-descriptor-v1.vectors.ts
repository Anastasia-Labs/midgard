/**
 * Golden vectors for `placeholderValidationTraceDescriptorV1`, shared by the
 * validation package, the node producer, the node's foreign import and the
 * fault-proof readers. The expected values were computed independently of the
 * TypeScript implementation (plain BLAKE2b-256 over the specified preimages).
 * Vectors are looked up by name.
 */
import type { MidgardRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";

export type PlaceholderValidationTraceDescriptorVector = Readonly<{
  name: string;
  eventKeyCbor: string;
  transactionCommitment: string;
  verdict: "accepted" | "rejected";
  rejectionReason: MidgardRejectionReasonV1 | null;
  /**
   * Present when the commitment was taken from a real canonical transaction
   * (by the existing proof-source commitment code), so producers that start
   * from transaction bytes can be checked against the same vector.
   */
  transaction?: Readonly<{
    canonicalTransactionCbor: string;
    sourceKind: "normal" | "forced";
  }>;
  expected: Readonly<{
    schemaVersion: number;
    machineVersion: number;
    stepCount: number;
    traceRoot: string;
    initialStateHash: string;
    terminalStateHash: string;
    rejectionCodeHash: string;
    /** Plutus Data encoding committed under the validation_traces root. */
    descriptorDataCbor: string;
  }>;
}>;

export const PLACEHOLDER_VALIDATION_TRACE_DESCRIPTOR_VECTORS: readonly PlaceholderValidationTraceDescriptorVector[] =
  [
    {
      name: "normal-transaction-accepted",
      eventKeyCbor:
        "d87b9f58209ba2a50e71208a7a515f57ca51133a0a4a6b8ca3ad5a456dc4c7bcdda5ad8684ff",
      transactionCommitment:
        "d94e1ed6628e41483ab52919dde718ced8cb35763a70c008042b7c635fdd3085",
      verdict: "accepted",
      rejectionReason: null,
      expected: {
        schemaVersion: 1,
        machineVersion: 1,
        stepCount: 1,
        traceRoot:
          "1235f4919256a981d2f6e2521d6fc0ce5b129e7747c80ee57ed53ed5c4638e0c",
        initialStateHash:
          "2db179885d48dbcd23fd10c45a810bd08a304c67cba39c6171cf9201a0fec1dd",
        terminalStateHash:
          "dbf76cf7cc76cc4f5388eb6644f32236098293a535da35bfa046c4dd7bc50169",
        rejectionCodeHash:
          "0000000000000000000000000000000000000000000000000000000000000000",
        descriptorDataCbor:
          "d8799f010158201235f4919256a981d2f6e2521d6fc0ce5b129e7747c80ee57ed53ed5c4638e0c0158202db179885d48dbcd23fd10c45a810bd08a304c67cba39c6171cf9201a0fec1dd5820dbf76cf7cc76cc4f5388eb6644f32236098293a535da35bfa046c4dd7bc50169d87a8058200000000000000000000000000000000000000000000000000000000000000000ff",
      },
      transaction: {
        canonicalTransactionCbor:
          "84018c5829815826825820111111111111111111111111111111111111111111111111111111111111111119000041805828815825a200581d60bcd664029efb845877fc4dd01b73ff91c06fb09d213c1e9572034bbc01820aa0002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff83586881586582582073d22eb02fa7e5e0f8ffc6137e1fbbabfc5e0e18edc1a80a22b58db82544701d5840770dc9e4f74a3258b72c6f85d253c8157c76abcf173d22815074d01f11c5b7e24a34e3262eb5e6d769511568d43d5aa58d83e1d9746dd02b6c3841819e82000e4180418000",
        sourceKind: "normal",
      },
    },
    {
      name: "forced-transaction-rejected",
      eventKeyCbor:
        "d87a9fd8799f5820444444444444444444444444444444444444444444444444444444444444444400ffff",
      transactionCommitment:
        "b3fc5511490989c1b10bbac7d2a6a23795d7b9130140e0550df3464c10a68680",
      verdict: "rejected",
      rejectionReason: "EmptyInputs",
      expected: {
        schemaVersion: 1,
        machineVersion: 1,
        stepCount: 1,
        traceRoot:
          "4e5a9937c76f225650f33ed78066c33bca61126ed6d89b8531f29227e00c720b",
        initialStateHash:
          "ea555f67e8abff3c0e2e7c6c15c474af0cb2b5535d4a6b93d3d8a58c2989e531",
        terminalStateHash:
          "d663f7ed22c2fb49c5e423489d91bf45446b3d199db5507383df560427ac80a1",
        rejectionCodeHash:
          "9c71cc54543951c53a94a959911cab6c8c2586f22a9f87c81479eae5b6d8a5a1",
        descriptorDataCbor:
          "d8799f010158204e5a9937c76f225650f33ed78066c33bca61126ed6d89b8531f29227e00c720b015820ea555f67e8abff3c0e2e7c6c15c474af0cb2b5535d4a6b93d3d8a58c2989e5315820d663f7ed22c2fb49c5e423489d91bf45446b3d199db5507383df560427ac80a1d87b8058209c71cc54543951c53a94a959911cab6c8c2586f22a9f87c81479eae5b6d8a5a1ff",
      },
      transaction: {
        canonicalTransactionCbor:
          "83018c5829815826825820111111111111111111111111111111111111111111111111111111111111111119000041805828815825a200581d60bcd664029efb845877fc4dd01b73ff91c06fb09d213c1e9572034bbc01820aa0002020418041804180582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff83586881586582582073d22eb02fa7e5e0f8ffc6137e1fbbabfc5e0e18edc1a80a22b58db82544701d5840770dc9e4f74a3258b72c6f85d253c8157c76abcf173d22815074d01f11c5b7e24a34e3262eb5e6d769511568d43d5aa58d83e1d9746dd02b6c3841819e82000e41804180",
        sourceKind: "forced",
      },
    },
    {
      name: "normal-accepted",
      eventKeyCbor:
        "d87b9f5820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaff",
      transactionCommitment:
        "1111111111111111111111111111111111111111111111111111111111111111",
      verdict: "accepted",
      rejectionReason: null,
      expected: {
        schemaVersion: 1,
        machineVersion: 1,
        stepCount: 1,
        traceRoot:
          "e1429e346e0502661a153eb7cb19103ae72878aebebe41e2796d373b548277c9",
        initialStateHash:
          "bec63c4d23d4a402b917cd0f90e35254e87d853fdb20a7d143edfe02ba2bc401",
        terminalStateHash:
          "24b636608c9a7f1744a272d7d9454d597208b67bbbef5c78a2cc79eeb116e771",
        rejectionCodeHash:
          "0000000000000000000000000000000000000000000000000000000000000000",
        descriptorDataCbor:
          "d8799f01015820e1429e346e0502661a153eb7cb19103ae72878aebebe41e2796d373b548277c9015820bec63c4d23d4a402b917cd0f90e35254e87d853fdb20a7d143edfe02ba2bc401582024b636608c9a7f1744a272d7d9454d597208b67bbbef5c78a2cc79eeb116e771d87a8058200000000000000000000000000000000000000000000000000000000000000000ff",
      },
    },
    {
      name: "forced-accepted",
      eventKeyCbor:
        "d87a9fd8799f5820bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb03ffff",
      transactionCommitment:
        "2222222222222222222222222222222222222222222222222222222222222222",
      verdict: "accepted",
      rejectionReason: null,
      expected: {
        schemaVersion: 1,
        machineVersion: 1,
        stepCount: 1,
        traceRoot:
          "e1d204609805dea190476519ab5882044e58bd48f291e326e6b4c79c8a24c792",
        initialStateHash:
          "dc220b2457a101a5396fa0ee5835ac012a767fef6283e1752fd1d38a14a5128a",
        terminalStateHash:
          "142ead4b3b72c6fe928977e51fd08e480e073a06eeac3a98091ae45796219880",
        rejectionCodeHash:
          "0000000000000000000000000000000000000000000000000000000000000000",
        descriptorDataCbor:
          "d8799f01015820e1d204609805dea190476519ab5882044e58bd48f291e326e6b4c79c8a24c792015820dc220b2457a101a5396fa0ee5835ac012a767fef6283e1752fd1d38a14a5128a5820142ead4b3b72c6fe928977e51fd08e480e073a06eeac3a98091ae45796219880d87a8058200000000000000000000000000000000000000000000000000000000000000000ff",
      },
    },
    {
      name: "forced-rejected",
      eventKeyCbor:
        "d87a9fd8799f5820cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00ffff",
      transactionCommitment:
        "3333333333333333333333333333333333333333333333333333333333333333",
      verdict: "rejected",
      rejectionReason: { InputNotFound: { source_kind: 0n, input_index: -1n } },
      expected: {
        schemaVersion: 1,
        machineVersion: 1,
        stepCount: 1,
        traceRoot:
          "75ef9ef3683a1e50f044d9c1c6c144829593a943b1e4ca0ddf79c52df2073dcd",
        initialStateHash:
          "cc23a018368bed67df51cc11d4efae9a442316c9bcef171cddb4778a739a1f58",
        terminalStateHash:
          "0cc7179a4b0cffaa0e7b40ce46443c5869281392897783fd4c0eda0337b55878",
        rejectionCodeHash:
          "92540c0ddc1150a85e4f87ce8be9affe086dd2c235dc5ddf3da712ae43bda343",
        descriptorDataCbor:
          "d8799f0101582075ef9ef3683a1e50f044d9c1c6c144829593a943b1e4ca0ddf79c52df2073dcd015820cc23a018368bed67df51cc11d4efae9a442316c9bcef171cddb4778a739a1f5858200cc7179a4b0cffaa0e7b40ce46443c5869281392897783fd4c0eda0337b55878d87b80582092540c0ddc1150a85e4f87ce8be9affe086dd2c235dc5ddf3da712ae43bda343ff",
      },
    },
    {
      name: "forced-rejected-min-fee",
      eventKeyCbor:
        "d87a9fd8799f5820cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00ffff",
      transactionCommitment:
        "3333333333333333333333333333333333333333333333333333333333333333",
      verdict: "rejected",
      rejectionReason: "FeeBelowMinimum",
      expected: {
        schemaVersion: 1,
        machineVersion: 1,
        stepCount: 1,
        traceRoot:
          "ce67f1fc723b2b46f427c170774bdc7e5c1b63d4f40c2ef34b4d7fb03842a50e",
        initialStateHash:
          "cc23a018368bed67df51cc11d4efae9a442316c9bcef171cddb4778a739a1f58",
        terminalStateHash:
          "de8c02ba02b3b4b6f4de8413678843df7344eb33faf6f34513173e6745434712",
        rejectionCodeHash:
          "1c58e368280379bf2a43565907240d90e71114cc8fc3422c0f9b1fdc58bd7bef",
        descriptorDataCbor:
          "d8799f01015820ce67f1fc723b2b46f427c170774bdc7e5c1b63d4f40c2ef34b4d7fb03842a50e015820cc23a018368bed67df51cc11d4efae9a442316c9bcef171cddb4778a739a1f585820de8c02ba02b3b4b6f4de8413678843df7344eb33faf6f34513173e6745434712d87b8058201c58e368280379bf2a43565907240d90e71114cc8fc3422c0f9b1fdc58bd7befff",
      },
    },
  ];

export const placeholderValidationTraceDescriptorVector = (
  name: string,
): PlaceholderValidationTraceDescriptorVector => {
  const vector = PLACEHOLDER_VALIDATION_TRACE_DESCRIPTOR_VECTORS.find(
    (candidate) => candidate.name === name,
  );
  if (vector === undefined) throw new Error(`no placeholder vector ${name}`);
  return vector;
};
