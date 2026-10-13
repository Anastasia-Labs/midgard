/**
 * Golden vectors for `placeholderValidationTraceDescriptorV1`, shared by the
 * validation package, the node producer, the node's foreign import and the
 * fault-proof readers. The expected values were computed independently of the
 * TypeScript implementation (plain BLAKE2b-256 over the specified preimages).
 * Vectors are looked up by name.
 */

export type PlaceholderValidationTraceDescriptorVector = Readonly<{
  name: string;
  eventKeyCbor: string;
  transactionCommitment: string;
  verdict: "accepted" | "rejected";
  rejectionCode: string | null;
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
      rejectionCode: null,
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
      rejectionCode: "E_EMPTY_INPUTS",
      expected: {
        schemaVersion: 1,
        machineVersion: 1,
        stepCount: 1,
        traceRoot:
          "5f05ebad9e56f126cd8cb3b3a78fd959010460e3ad9b56393acdcdfbdfbc91a4",
        initialStateHash:
          "ea555f67e8abff3c0e2e7c6c15c474af0cb2b5535d4a6b93d3d8a58c2989e531",
        terminalStateHash:
          "3017eda56a19f1163d42ad9092f10184601be1c73a0e3f6311dec9ad2417c9ed",
        rejectionCodeHash:
          "fb4133ff04336d3ef299bf76f6e6b4267be9a7931285e4fbc9f848e66c3624d8",
        descriptorDataCbor:
          "d8799f010158205f05ebad9e56f126cd8cb3b3a78fd959010460e3ad9b56393acdcdfbdfbc91a4015820ea555f67e8abff3c0e2e7c6c15c474af0cb2b5535d4a6b93d3d8a58c2989e53158203017eda56a19f1163d42ad9092f10184601be1c73a0e3f6311dec9ad2417c9edd87b805820fb4133ff04336d3ef299bf76f6e6b4267be9a7931285e4fbc9f848e66c3624d8ff",
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
      rejectionCode: null,
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
      rejectionCode: null,
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
      rejectionCode: "E_INPUT_NOT_FOUND",
      expected: {
        schemaVersion: 1,
        machineVersion: 1,
        stepCount: 1,
        traceRoot:
          "5eead76426c3652decc02747e2edfbeecee117da455c10721681b39133ee4128",
        initialStateHash:
          "cc23a018368bed67df51cc11d4efae9a442316c9bcef171cddb4778a739a1f58",
        terminalStateHash:
          "b88e89c7c559eaa084a6ff897a7083463585fe548def557d29e3f4f8cb8976d8",
        rejectionCodeHash:
          "7010e02d40fd5e53e846ed857627684cd0eb66ea6bc07979257063505807a187",
        descriptorDataCbor:
          "d8799f010158205eead76426c3652decc02747e2edfbeecee117da455c10721681b39133ee4128015820cc23a018368bed67df51cc11d4efae9a442316c9bcef171cddb4778a739a1f585820b88e89c7c559eaa084a6ff897a7083463585fe548def557d29e3f4f8cb8976d8d87b8058207010e02d40fd5e53e846ed857627684cd0eb66ea6bc07979257063505807a187ff",
      },
    },
    {
      name: "forced-rejected-min-fee",
      eventKeyCbor:
        "d87a9fd8799f5820cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00ffff",
      transactionCommitment:
        "3333333333333333333333333333333333333333333333333333333333333333",
      verdict: "rejected",
      rejectionCode: "E_MIN_FEE",
      expected: {
        schemaVersion: 1,
        machineVersion: 1,
        stepCount: 1,
        traceRoot:
          "043b8baac8e92c3ce25036cb18e2558fbbc74eefa034c51dc6107ecac1857906",
        initialStateHash:
          "cc23a018368bed67df51cc11d4efae9a442316c9bcef171cddb4778a739a1f58",
        terminalStateHash:
          "aa9a4d5a53f6e849826785a3685341c6d0927d2e70cbf451bacbfa83d0964812",
        rejectionCodeHash:
          "46055c0fdb633167ed024b753cfe257f68a4845f3843ffca361e865cbdfc2809",
        descriptorDataCbor:
          "d8799f01015820043b8baac8e92c3ce25036cb18e2558fbbc74eefa034c51dc6107ecac1857906015820cc23a018368bed67df51cc11d4efae9a442316c9bcef171cddb4778a739a1f585820aa9a4d5a53f6e849826785a3685341c6d0927d2e70cbf451bacbfa83d0964812d87b80582046055c0fdb633167ed024b753cfe257f68a4845f3843ffca361e865cbdfc2809ff",
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
