/**
 * The synthetic native chain-sync helper executable. It runs one helper
 * session per process for streams, and every exact-point query as one session
 * of the persistent exact-point helper, reading the fixture's control files.
 */
export const syntheticNativeHelperScript = ({
  controlPath,
  registryPath,
  counterPath,
  tipPath,
  queryLogPath,
  nativeTipBaseDepth,
  nativeStreamInitialAcknowledgement,
}: Readonly<{
  controlPath: string;
  registryPath: string;
  counterPath: string;
  tipPath: string;
  queryLogPath: string;
  nativeTipBaseDepth: number;
  nativeStreamInitialAcknowledgement: boolean;
}>): string => `#!${process.execPath}
import { appendFileSync, existsSync, readFileSync, renameSync, statSync, writeFileSync } from 'node:fs';
import { createHash } from 'node:crypto';
import { createInterface } from 'node:readline';
const reader=createInterface({input:process.stdin,crlfDelay:Infinity});
// The producer replaces these files atomically. Poll metadata every tick, but
// parse growing block registries only when their file identity changes.
const cachedJson=path=>{
  let identity;
  let value;
  return ()=>{
    const stat=statSync(path,{bigint:true});
    const next=[stat.dev,stat.ino,stat.size,stat.mtimeNs,stat.ctimeNs].join(':');
    if(next!==identity){
      value=JSON.parse(readFileSync(path,'utf8'));
      identity=next;
    }
    return value;
  };
};
const readControl=cachedJson(${JSON.stringify(controlPath)});
const readBlocks=cachedJson(${JSON.stringify(registryPath)});
const canonical=value=>Array.isArray(value)?value.map(canonical):value!==null&&typeof value==='object'?Object.fromEntries(Object.keys(value).sort().map(key=>[key,canonical(value[key])])):value;
// One helper session: a whole per-process helper, or one exact-point session
// of the persistent service with its own startup, tip and command cursor.
const runSession=(line,io)=>{
const startup=JSON.parse(line);
const initialControl=readControl();
if(initialControl.closed) throw new Error('Synthetic fixture is closed');
if(startup.operation.kind!=='exact_point'&&startup.operation.kind!=='stream') throw new Error('Unknown synthetic native operation');
const exact=startup.operation.kind==='exact_point';
const block=exact?readBlocks().find(block=>block.point.blockHash===startup.operation.target.blockHash):null;
if(initialControl.canonicalBranchSelected) {
  const requested=exact?startup.operation.target:startup.intersection;
  const registered=requested.kind==='origin'||readBlocks().some(candidate=>candidate.point.blockHash===requested.blockHash||candidate.parentPoint.blockHash===requested.blockHash);
  if(!registered||exact&&!block){io.out(JSON.stringify({code:'intersection_failed',kind:'error',schemaVersion:startup.schemaVersion}));io.exit(1);}
}
if(exact&&!block) throw new Error('Unknown synthetic fixture block');
let legacyTip;
if(initialControl.mode==='query_counter') {
  if(exact) {
    const query=Number(readFileSync(${JSON.stringify(counterPath)},'utf8'))+1;
    writeFileSync(${JSON.stringify(counterPath)},String(query));
    legacyTip={kind:'point',blockHash:createHash('sha256').update('synthetic-tip-'+query).digest('hex'),blockNo:String(BigInt(block.point.blockNo)+BigInt(${nativeTipBaseDepth}+query)),slot:String(BigInt(block.point.slot)+BigInt(${Math.max(600, nativeTipBaseDepth)}+query))};
    writeFileSync(${JSON.stringify(tipPath + ".next")}+process.pid,JSON.stringify(legacyTip));
    renameSync(${JSON.stringify(tipPath + ".next")}+process.pid,${JSON.stringify(tipPath)});
  } else {
    legacyTip=existsSync(${JSON.stringify(tipPath)})?JSON.parse(readFileSync(${JSON.stringify(tipPath)},'utf8')):{kind:'point',...initialControl.tip};
  }
}
const tipAt=control=>control.mode==='controlled'?{kind:'point',...control.tip}:legacyTip;
const emit=value=>io.out(JSON.stringify(canonical(value)));
const emitBlock=(block,tip)=>emit({blockHash:block.point.blockHash,blockNo:block.point.blockNo,slot:block.point.slot,blockType:'7',kind:'roll_forward',prevHash:block.parentPoint.blockHash,rawBlockCbor:block.nativeBlock.rawBlockCbor,schemaVersion:startup.schemaVersion,tip});
const startupDigest=createHash('sha256').update(line,'utf8').digest('hex');
const tip=tipAt(initialControl);
emit({authorityNodeId:startup.authorityNodeId,currentTip:tip,genesisIdentitySha256:startup.genesisIdentitySha256,kind:'ready',network:startup.network,networkMagic:startup.networkMagic,operation:startup.operation,schemaVersion:startup.schemaVersion,selectedIntersection:startup.intersection,socketPath:startup.socketPath,startupDigest});
if(!exact&&${JSON.stringify(nativeStreamInitialAcknowledgement)})
  emit({schemaVersion:startup.schemaVersion,kind:'roll_backward',point:startup.intersection,tip});
if(exact) {
  appendFileSync(${JSON.stringify(queryLogPath)},JSON.stringify({startupDigest,target:{blockHash:block.point.blockHash,blockNo:block.point.blockNo,slot:block.point.slot},tip:{blockHash:tip.blockHash,blockNo:tip.blockNo,slot:tip.slot}})+'\\n');
  emitBlock(block,tip);
}
let position=startup.intersection;
let commandCursor=initialControl.commands.length;
io.every(()=>{
  const control=readControl();
  if(control.closed) process.exit(0);
  if(exact&&!control.canonicalBranchSelected) return;
  const tip=tipAt(control);
  while(commandCursor<control.commands.length) {
    const command=control.commands[commandCursor++];
    if(command.kind==='exit') {
      io.err('Synthetic native stream exit requested\\n');
      io.exit(command.exitCode);
    }
    position=command.point==='origin'?{kind:'origin'}:{kind:'point',blockHash:command.point.blockHash,slot:command.point.slot};
    emit({schemaVersion:startup.schemaVersion,kind:'roll_backward',point:position,tip});
  }
  if(exact) return;
  const blocks=readBlocks();
  const next=position.kind==='origin'?blocks[0]:blocks.find(block=>block.parentPoint.blockHash===position.blockHash&&block.parentPoint.slot===position.slot);
  if(!next||BigInt(next.point.blockNo)>BigInt(tip.blockNo)||BigInt(next.point.slot)>BigInt(tip.slot)) return;
  emitBlock(next,tip);
  position={kind:'point',blockHash:next.point.blockHash,slot:next.point.slot};
});
};
// A held exact-point session waits before readiness; a closed fixture still
// ends the helper.
const held=line=>{
  const control=readControl();
  if(control.closed) process.exit(0);
  return control.exactQueriesHeld===true&&JSON.parse(line).operation?.kind==='exact_point';
};
if(process.argv[2]!=='--exact-point-service') {
  const line=await new Promise(resolve=>reader.once('line',resolve));
  while(held(line)) await new Promise(resolve=>setTimeout(resolve,10));
  runSession(line,{out:value=>process.stdout.write(value+'\\n'),err:text=>process.stderr.write(text),exit:code=>process.exit(code),every:tick=>setInterval(tick,10)});
} else {
  // The persistent exact-point helper: the same session body per framed
  // open, a session's process exit becomes its end frame, and a closed
  // fixture still ends the whole helper as it ended every helper process.
  class SessionExit { constructor(code){ this.code=code; } }
  const sessions=new Map();
  const frame=text=>process.stdout.write(text);
  reader.on('line',request=>{
    const [verb,id]=request.split(' ',2);
    if(verb==='close') { sessions.get(id)?.end(0); return; }
    if(verb!=='open') process.exit(65);
    const session={ended:false,timer:undefined};
    const run=work=>{
      try { work(); }
      catch(error) {
        if(error instanceof SessionExit) session.end(error.code);
        else { session.io.err(String(error?.stack??error)+'\\n'); session.end(1); }
      }
    };
    session.end=code=>{
      if(session.ended) return;
      session.ended=true;
      clearInterval(session.timer);
      sessions.delete(id);
      frame('end '+id+' '+code+'\\n');
    };
    session.io={
      out:value=>{ if(!session.ended) frame('out '+id+' '+value+'\\n'); },
      err:text=>{ if(!session.ended) frame('err '+id+' '+Buffer.from(text).toString('base64')+'\\n'); },
      exit:code=>{ throw new SessionExit(code); },
      every:tick=>{ if(!session.ended) session.timer=setInterval(()=>run(tick),10); },
    };
    sessions.set(id,session);
    const line=request.slice(verb.length+id.length+2);
    const start=()=>{
      if(session.ended) return;
      run(()=>{
        if(held(line)) session.timer=setTimeout(start,10);
        else runSession(line,session.io);
      });
    };
    start();
  });
  reader.on('close',()=>process.exit(0));
}
`;
