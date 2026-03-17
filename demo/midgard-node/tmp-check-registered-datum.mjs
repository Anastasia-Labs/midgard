import * as SDK from '@al-ft/midgard-sdk';
import { Data } from '@lucid-evolution/lucid';

const activationTime = 1772381004030n;
console.log('schema', SDK.RegisteredOperatorDatumSchema ?? 'no schema');
console.log('datum export type', typeof SDK.RegisteredOperatorDatum);

try {
  const x = Data.to({ registrationTime: activationTime }, SDK.RegisteredOperatorDatum);
  console.log('Data.to direct', x);
} catch (e) {
  console.log('Data.to direct err', e?.message);
}

try {
  const y = Data.castTo({ registrationTime: activationTime }, SDK.RegisteredOperatorDatum);
  console.log('Data.castTo', y);
} catch (e) {
  console.log('Data.castTo err', e?.message);
}

try {
  const z = Data.to({ activation_time: activationTime }, Data.Object({ activation_time: Data.Integer() }));
  console.log('aiken data', z);
} catch (e) {
  console.log('aiken data err', e?.message);
}
