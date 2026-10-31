export const fail = (message) => {
  throw new Error(`transaction-root-v1 fixture generation failed: ${message}`);
};

const isRecord = (value) =>
  value !== null && typeof value === "object" && !Array.isArray(value);

export const exactKeys = (value, keys, label) => {
  if (!isRecord(value)) fail(`${label} must be an object`);
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (JSON.stringify(actual) !== JSON.stringify(expected)) {
    fail(`${label} must contain exactly ${keys.join(", ")}`);
  }
  return value;
};

export const nonEmptyString = (value, label) => {
  if (typeof value !== "string" || value.length === 0) {
    fail(`${label} must be a non-empty string`);
  }
  return value;
};

export const hex = (value, label, byteLength) => {
  nonEmptyString(value, label);
  if (!/^[0-9a-f]+$/u.test(value) || value.length % 2 !== 0) {
    fail(`${label} must be lowercase even-length hexadecimal`);
  }
  if (byteLength !== undefined && value.length !== byteLength * 2) {
    fail(`${label} must be exactly ${byteLength.toString()} bytes`);
  }
  return Buffer.from(value, "hex");
};

export const decimal = (value, label) => {
  if (typeof value !== "string" || !/^(?:0|-?[1-9][0-9]*)$/u.test(value)) {
    fail(`${label} must be a canonical decimal integer string`);
  }
  return BigInt(value);
};

export const enumValue = (value, allowed, label) => {
  if (typeof value !== "string" || !allowed.includes(value)) {
    fail(`${label} must be one of ${allowed.join(", ")}`);
  }
  return value;
};

export const parseCanonicalTransaction = (value, label) => {
  exactKeys(
    value,
    ["name", "version", "validity", "body", "witnessSet"],
    label,
  );
  const name = nonEmptyString(value.name, `${label}.name`);
  exactKeys(
    value.body,
    [
      "spendInputsPreimageCbor",
      "referenceInputsPreimageCbor",
      "outputsPreimageCbor",
      "fee",
      "validityIntervalStart",
      "validityIntervalEnd",
      "requiredObserversPreimageCbor",
      "requiredSignersPreimageCbor",
      "mintPreimageCbor",
      "scriptIntegrityHash",
      "auxiliaryDataHash",
      "networkId",
    ],
    `${label}.body`,
  );
  exactKeys(
    value.witnessSet,
    [
      "addrTxWitsPreimageCbor",
      "scriptTxWitsPreimageCbor",
      "redeemerTxWitsPreimageCbor",
    ],
    `${label}.witnessSet`,
  );
  const body = value.body;
  const witnessSet = value.witnessSet;
  return {
    name,
    transaction: {
      version: decimal(value.version, `${label}.version`),
      validity: enumValue(value.validity, ["TxIsValid"], `${label}.validity`),
      body: {
        spendInputsPreimageCbor: hex(
          body.spendInputsPreimageCbor,
          `${label}.body.spendInputsPreimageCbor`,
        ),
        referenceInputsPreimageCbor: hex(
          body.referenceInputsPreimageCbor,
          `${label}.body.referenceInputsPreimageCbor`,
        ),
        outputsPreimageCbor: hex(
          body.outputsPreimageCbor,
          `${label}.body.outputsPreimageCbor`,
        ),
        fee: decimal(body.fee, `${label}.body.fee`),
        validityIntervalStart: decimal(
          body.validityIntervalStart,
          `${label}.body.validityIntervalStart`,
        ),
        validityIntervalEnd: decimal(
          body.validityIntervalEnd,
          `${label}.body.validityIntervalEnd`,
        ),
        requiredObserversPreimageCbor: hex(
          body.requiredObserversPreimageCbor,
          `${label}.body.requiredObserversPreimageCbor`,
        ),
        requiredSignersPreimageCbor: hex(
          body.requiredSignersPreimageCbor,
          `${label}.body.requiredSignersPreimageCbor`,
        ),
        mintPreimageCbor: hex(
          body.mintPreimageCbor,
          `${label}.body.mintPreimageCbor`,
        ),
        scriptIntegrityHash: hex(
          body.scriptIntegrityHash,
          `${label}.body.scriptIntegrityHash`,
          32,
        ),
        auxiliaryDataHash: hex(
          body.auxiliaryDataHash,
          `${label}.body.auxiliaryDataHash`,
          32,
        ),
        networkId: decimal(body.networkId, `${label}.body.networkId`),
      },
      witnessSet: {
        addrTxWitsPreimageCbor: hex(
          witnessSet.addrTxWitsPreimageCbor,
          `${label}.witnessSet.addrTxWitsPreimageCbor`,
        ),
        scriptTxWitsPreimageCbor: hex(
          witnessSet.scriptTxWitsPreimageCbor,
          `${label}.witnessSet.scriptTxWitsPreimageCbor`,
        ),
        redeemerTxWitsPreimageCbor: hex(
          witnessSet.redeemerTxWitsPreimageCbor,
          `${label}.witnessSet.redeemerTxWitsPreimageCbor`,
        ),
      },
    },
  };
};

export const toJsonTransaction = (parsed) => ({
  version: parsed.version.toString(),
  validity: parsed.validity,
  body: Object.fromEntries(
    Object.entries(parsed.body).map(([key, value]) => [
      key,
      value instanceof Buffer ? value.toString("hex") : value.toString(),
    ]),
  ),
  witnessSet: Object.fromEntries(
    Object.entries(parsed.witnessSet).map(([key, value]) => [
      key,
      value.toString("hex"),
    ]),
  ),
});

export const aikenBytes = (value) => `#"${value}"`;

export const aikenInt = (value) => value.toString();
