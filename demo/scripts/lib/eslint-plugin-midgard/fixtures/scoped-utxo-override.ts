declare const lucid: any;
declare const first: any;
declare const second: any;
declare const address: string;
declare const build: (lucid: unknown) => Promise<unknown>;

export const leaks = async () => {
  // ruleid: midgard/scoped-utxo-override
  lucid.overrideUTxOs(await lucid.utxosAt(address));
  return build(lucid);
};

export const releasesAnotherInstance = async () => {
  try {
    // ruleid: midgard/scoped-utxo-override
    first.overrideUTxOs([]);
    return await build(first);
  } finally {
    second.clearUTxOOverride();
  }
};

export const releasesBeforePinning = () => {
  lucid.clearUTxOOverride();
  // ruleid: midgard/scoped-utxo-override
  lucid.overrideUTxOs([]);
};

export const scoped = async () => {
  try {
    // ok: midgard/scoped-utxo-override
    lucid.overrideUTxOs(await lucid.utxosAt(address));
    return await build(lucid);
  } finally {
    lucid.clearUTxOOverride();
  }
};

export const leaksThroughCall = async () => {
  const overrideUTxOs = lucid.overrideUTxOs;
  // ruleid: midgard/scoped-utxo-override
  overrideUTxOs.call(lucid, await lucid.utxosAt(address));
  return build(lucid);
};

export const leaksThroughApply = async () => {
  // ruleid: midgard/scoped-utxo-override
  lucid.overrideUTxOs.apply(lucid, [await lucid.utxosAt(address)]);
  return build(lucid);
};

export const callReleasesAnotherInstance = async () => {
  try {
    // ruleid: midgard/scoped-utxo-override
    first.overrideUTxOs.call(first, []);
    return await build(first);
  } finally {
    second.clearUTxOOverride.call(second);
  }
};

export const scopedThroughCall = async () => {
  const overrideUTxOs = lucid.overrideUTxOs;
  try {
    // ok: midgard/scoped-utxo-override
    overrideUTxOs.call(lucid, await lucid.utxosAt(address));
    return await build(lucid);
  } finally {
    lucid.clearUTxOOverride();
  }
};

export const scopedThroughApply = async () => {
  try {
    // ok: midgard/scoped-utxo-override
    lucid.overrideUTxOs.apply(lucid, [[]]);
    return await build(lucid);
  } finally {
    lucid.clearUTxOOverride.apply(lucid, []);
  }
};
