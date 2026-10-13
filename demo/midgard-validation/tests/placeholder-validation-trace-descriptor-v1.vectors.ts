/**
 * Golden vectors for `placeholderValidationTraceDescriptorV1`, shared by the
 * validation package, the node producer, the node's foreign import and the
 * fault-proof readers. The expected values were computed independently of the
 * TypeScript implementation (plain BLAKE2b-256 over the specified preimages).
 */

export type PlaceholderValidationTraceDescriptorVector = Readonly<{
  name: string;
  eventKeyCbor: string;
  transactionCommitment: string;
  verdict: "accepted" | "rejected";
  rejectionCode: string | null;
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
