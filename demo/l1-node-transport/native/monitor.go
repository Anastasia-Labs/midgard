package main

import (
	"errors"

	gcbor "github.com/blinklabs-io/gouroboros/cbor"
	"github.com/blinklabs-io/gouroboros/protocol"
	"github.com/blinklabs-io/gouroboros/protocol/localtxmonitor"
)

// conwayEra is the hard-fork era index of Conway.
const conwayEra = 6

// genTxID is a hard-fork-combinator transaction id: the era index and the
// transaction hash. The node matches ids by hash alone, so any Shelley-based
// era index finds a transaction of any Shelley-based era.
type genTxID struct {
	gcbor.StructAsArray
	Era  uint64
	Hash []byte
}

// msgHasTxHFC is MsgHasTx as the node decodes it: [7, [era, txId]]. The
// stock gouroboros message sends the bare hash, which the node refuses by
// closing the connection.
type msgHasTxHFC struct {
	protocol.MessageBase
	TxID genTxID
}

// monitorClient drives LocalTxMonitor: each call acquires a fresh mempool
// snapshot, asks one question and releases the snapshot.
type monitorClient struct {
	raw  *rawProtocol
	node *nodeConn
}

func (c *monitorClient) await() (protocol.Message, error) {
	select {
	case msg := <-c.raw.messages:
		return msg, nil
	case <-c.node.dead:
		return nil, c.node.deathCause()
	}
}

func (c *monitorClient) snapshot(ask protocol.Message, read func(protocol.Message) error) error {
	if err := c.raw.send(localtxmonitor.NewMsgAcquire()); err != nil {
		return err
	}
	msg, err := c.await()
	if err != nil {
		return err
	}
	if _, ok := msg.(*localtxmonitor.MsgAcquired); !ok {
		return errors.New("unexpected reply to MsgAcquire")
	}
	if err := c.raw.send(ask); err != nil {
		return err
	}
	if msg, err = c.await(); err != nil {
		return err
	}
	if err := read(msg); err != nil {
		return err
	}
	return c.raw.send(localtxmonitor.NewMsgRelease())
}

// hasTx asks under the Conway era index: the node matches ids by hash, so
// this finds a transaction of any Shelley-based era.
func (c *monitorClient) hasTx(txID []byte) (bool, error) {
	ask := &msgHasTxHFC{
		MessageBase: protocol.MessageBase{MessageType: localtxmonitor.MessageTypeHasTx},
		TxID:        genTxID{Era: conwayEra, Hash: txID},
	}
	var has bool
	err := c.snapshot(ask, func(msg protocol.Message) error {
		reply, ok := msg.(*localtxmonitor.MsgReplyHasTx)
		if !ok {
			return errors.New("unexpected reply to MsgHasTx")
		}
		has = reply.Result
		return nil
	})
	return has, err
}

func (c *monitorClient) sizes() (capacity, size, count uint32, err error) {
	err = c.snapshot(localtxmonitor.NewMsgGetSizes(), func(msg protocol.Message) error {
		reply, ok := msg.(*localtxmonitor.MsgReplyGetSizes)
		if !ok {
			return errors.New("unexpected reply to MsgGetSizes")
		}
		capacity, size, count = reply.Result.Capacity, reply.Result.Size, reply.Result.NumberOfTxs
		return nil
	})
	return capacity, size, count, err
}
