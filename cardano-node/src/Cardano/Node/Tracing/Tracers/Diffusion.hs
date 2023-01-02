{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Tracing.Tracers.Diffusion
  () where

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import qualified Codec.CBOR.Term as CBOR
import           Data.Aeson (Value (String), (.=))
import           Data.Text (pack)
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket
import           Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import           Text.Show

import           Cardano.Node.Configuration.TopologyP2P (UseLedger (..))

import qualified Ouroboros.Network.Diffusion as ND
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers (..), PoolStake (..),
                   TraceLedgerPeers (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (Message (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import           Ouroboros.Network.Snocket (LocalAddress (..))


--------------------------------------------------------------------------------
-- Mux Tracer
--------------------------------------------------------------------------------

instance (LogFormatting peer, Show peer) =>
    LogFormatting (WithMuxBearer peer MuxTrace) where
    forMachine dtal (WithMuxBearer b ev) =
      mconcat [ "kind" .= String "MuxTrace"
              , "bearer" .= forMachine dtal b
              , "event" .= showT ev ]
    forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev

instance MetaTrace tr => MetaTrace (WithMuxBearer peer tr) where
    namespaceFor (WithMuxBearer _peer obj) = (nsCast . namespaceFor) obj
    severityFor ns = severityFor (nsCast ns)
    privacyFor ns = privacyFor (nsCast ns)
    detailsFor ns = detailsFor (nsCast ns)
    documentFor ns = documentFor (nsCast ns :: Namespace tr)
    metricsDocFor ns = metricsDocFor (nsCast ns :: Namespace tr)
    allNamespaces = map nsCast (allNamespaces :: [Namespace tr])

instance MetaTrace MuxTrace where
    namespaceFor MuxTraceRecvHeaderStart {}       =
      Namespace [] ["RecvHeaderStart"]
    namespaceFor MuxTraceRecvHeaderEnd {}         =
      Namespace [] ["RecvHeaderEnd"]
    namespaceFor MuxTraceRecvStart {}             =
      Namespace [] ["RecvStart"]
    namespaceFor MuxTraceRecvEnd {}               =
      Namespace [] ["RecvEnd"]
    namespaceFor MuxTraceSendStart {}             =
      Namespace [] ["SendStart"]
    namespaceFor MuxTraceSendEnd                  =
      Namespace [] ["SendEnd"]
    namespaceFor MuxTraceState {}                 =
      Namespace [] ["State"]
    namespaceFor MuxTraceCleanExit {}             =
      Namespace [] ["CleanExit"]
    namespaceFor MuxTraceExceptionExit {}         =
      Namespace [] ["ExceptionExit"]
    namespaceFor MuxTraceChannelRecvStart {}      =
      Namespace [] ["ChannelRecvStart"]
    namespaceFor MuxTraceChannelRecvEnd {}        =
      Namespace [] ["ChannelRecvEnd"]
    namespaceFor MuxTraceChannelSendStart {}      =
      Namespace [] ["ChannelSendStart"]
    namespaceFor MuxTraceChannelSendEnd {}        =
      Namespace [] ["ChannelSendEnd"]
    namespaceFor MuxTraceHandshakeStart           =
      Namespace [] ["HandshakeStart"]
    namespaceFor MuxTraceHandshakeClientEnd {}    =
      Namespace [] ["HandshakeClientEnd"]
    namespaceFor MuxTraceHandshakeServerEnd       =
      Namespace [] ["HandshakeServerEnd"]
    namespaceFor MuxTraceHandshakeClientError {}  =
      Namespace [] ["HandshakeClientError"]
    namespaceFor MuxTraceHandshakeServerError {}  =
      Namespace [] ["HandshakeServerError"]
    namespaceFor MuxTraceRecvDeltaQObservation {} =
      Namespace [] ["RecvDeltaQObservation"]
    namespaceFor MuxTraceRecvDeltaQSample {}      =
      Namespace [] ["RecvDeltaQSample"]
    namespaceFor MuxTraceSDUReadTimeoutException  =
      Namespace [] ["SDUReadTimeoutException"]
    namespaceFor MuxTraceSDUWriteTimeoutException =
      Namespace [] ["SDUWriteTimeoutException"]
    namespaceFor MuxTraceStartEagerly {}          =
      Namespace [] ["StartEagerly"]
    namespaceFor MuxTraceStartOnDemand {}         =
      Namespace [] ["StartOnDemand"]
    namespaceFor MuxTraceStartedOnDemand {}       =
      Namespace [] ["StartedOnDemand"]
    namespaceFor MuxTraceTerminating {}           =
      Namespace [] ["Terminating"]
    namespaceFor MuxTraceShutdown {}              =
      Namespace [] ["Shutdown"]
    namespaceFor MuxTraceTCPInfo {}               =
      Namespace [] ["TCPInfo"]

    severityFor (Namespace _ ["RecvHeaderStart"]) _       = Just Debug
    severityFor (Namespace _ ["RecvHeaderEnd"]) _         = Just Debug
    severityFor (Namespace _ ["RecvStart"]) _             = Just Debug
    severityFor (Namespace _ ["RecvEnd"]) _               = Just Debug
    severityFor (Namespace _ ["SendStart"]) _             = Just Debug
    severityFor (Namespace _ ["SendEnd"]) _               = Just Debug
    severityFor (Namespace _ ["State"]) _                 = Just Info
    severityFor (Namespace _ ["CleanExit"]) _             = Just Notice
    severityFor (Namespace _ ["ExceptionExit"]) _         = Just Notice
    severityFor (Namespace _ ["ChannelRecvStart"]) _      = Just Debug
    severityFor (Namespace _ ["ChannelRecvEnd"]) _        = Just Debug
    severityFor (Namespace _ ["ChannelSendStart"]) _      = Just Debug
    severityFor (Namespace _ ["ChannelSendEnd"]) _        = Just Debug
    severityFor (Namespace _ ["HandshakeStart"]) _        = Just Debug
    severityFor (Namespace _ ["HandshakeClientEnd"]) _    = Just Info
    severityFor (Namespace _ ["HandshakeServerEnd"]) _    = Just Debug
    severityFor (Namespace _ ["HandshakeClientError"]) _  = Just Error
    severityFor (Namespace _ ["HandshakeServerError"]) _  = Just Error
    severityFor (Namespace _ ["RecvDeltaQObservation"]) _ = Just Debug
    severityFor (Namespace _ ["RecvDeltaQSample"]) _      = Just Debug
    severityFor (Namespace _ ["SDUReadTimeoutException"]) _  = Just Notice
    severityFor (Namespace _ ["SDUWriteTimeoutException"]) _ = Just Notice
    severityFor (Namespace _ ["StartEagerly"]) _          = Just Debug
    severityFor (Namespace _ ["StartOnDemand"]) _         = Just Debug
    severityFor (Namespace _ ["StartedOnDemand"]) _       = Just Debug
    severityFor (Namespace _ ["Terminating"]) _           = Just Debug
    severityFor (Namespace _ ["Shutdown"]) _              = Just Debug
    severityFor (Namespace _ ["TCPInfo"]) _               = Just Debug
    severityFor _ _ = Nothing

    documentFor (Namespace _ ["RecvHeaderStart"])       = Just
      "Bearer receive header start."
    documentFor (Namespace _ ["RecvHeaderEnd"])         = Just
      "Bearer receive header end."
    documentFor (Namespace _ ["RecvStart"])             = Just
      "Bearer receive start."
    documentFor (Namespace _ ["RecvEnd"])               = Just
      "Bearer receive end."
    documentFor (Namespace _ ["SendStart"])             = Just
      "Bearer send start."
    documentFor (Namespace _ ["SendEnd"])               = Just
      "Bearer send end."
    documentFor (Namespace _ ["State"])                 = Just
      "State."
    documentFor (Namespace _ ["CleanExit"])             = Just
      "Miniprotocol terminated cleanly."
    documentFor (Namespace _ ["ExceptionExit"])         = Just
      "Miniprotocol terminated with exception."
    documentFor (Namespace _ ["ChannelRecvStart"])      = Just
      "Channel receive start."
    documentFor (Namespace _ ["ChannelRecvEnd"])        = Just
      "Channel receive end."
    documentFor (Namespace _ ["ChannelSendStart"])      = Just
      "Channel send start."
    documentFor (Namespace _ ["ChannelSendEnd"])        = Just
      "Channel send end."
    documentFor (Namespace _ ["HandshakeStart"])        = Just
      "Handshake start."
    documentFor (Namespace _ ["HandshakeClientEnd"])    = Just
      "Handshake client end."
    documentFor (Namespace _ ["HandshakeServerEnd"])    = Just
      "Handshake server end."
    documentFor (Namespace _ ["HandshakeClientError"])  = Just
      "Handshake client error."
    documentFor (Namespace _ ["HandshakeServerError"])  = Just
      "Handshake server error."
    documentFor (Namespace _ ["RecvDeltaQObservation"]) = Just
      "Bearer DeltaQ observation."
    documentFor (Namespace _ ["RecvDeltaQSample"])      = Just
      "Bearer DeltaQ sample."
    documentFor (Namespace _ ["SDUReadTimeoutException"])  = Just
      "Timed out reading SDU."
    documentFor (Namespace _ ["SDUWriteTimeoutException"]) = Just
      "Timed out writing SDU."
    documentFor (Namespace _ ["StartEagerly"])          = Just
      "Eagerly started."
    documentFor (Namespace _ ["StartOnDemand"])         = Just
      "Preparing to start."
    documentFor (Namespace _ ["StartedOnDemand"])       = Just
      "Started on demand."
    documentFor (Namespace _ ["Terminating"])           = Just
      "Terminating."
    documentFor (Namespace _ ["Shutdown"])              = Just
      "Mux shutdown."
    documentFor (Namespace _ ["TCPInfo"])               = Just
      "TCPInfo."
    documentFor _ = Nothing

    allNamespaces = [
        Namespace [] ["RecvHeaderStart"]
      , Namespace [] ["RecvHeaderEnd"]
      , Namespace [] ["RecvStart"]
      , Namespace [] ["RecvEnd"]
      , Namespace [] ["SendStart"]
      , Namespace [] ["SendEnd"]
      , Namespace [] ["State"]
      , Namespace [] ["CleanExit"]
      , Namespace [] ["ExceptionExit"]
      , Namespace [] ["ChannelRecvStart"]
      , Namespace [] ["ChannelRecvEnd"]
      , Namespace [] ["ChannelSendStart"]
      , Namespace [] ["ChannelSendEnd"]
      , Namespace [] ["HandshakeStart"]
      , Namespace [] ["HandshakeClientEnd"]
      , Namespace [] ["HandshakeServerEnd"]
      , Namespace [] ["HandshakeClientError"]
      , Namespace [] ["HandshakeServerError"]
      , Namespace [] ["RecvDeltaQObservation"]
      , Namespace [] ["RecvDeltaQSample"]
      , Namespace [] ["SDUReadTimeoutException"]
      , Namespace [] ["SDUWriteTimeoutException"]
      , Namespace [] ["StartEagerly"]
      , Namespace [] ["StartOnDemand"]
      , Namespace [] ["StartedOnDemand"]
      , Namespace [] ["Terminating"]
      , Namespace [] ["Shutdown"]
      , Namespace [] ["TCPInfo"]
      ]

--------------------------------------------------------------------------------
-- Handshake Tracer
--------------------------------------------------------------------------------

instance (Show adr, Show ver) => LogFormatting (NtN.HandshakeTr adr ver) where
  forMachine _dtal (WithMuxBearer b ev) =
    mconcat [ "kind" .= String "HandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]
  forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev

instance MetaTrace (AnyMessageAndAgency (HS.Handshake vNumber term)) where


-- severityHandshake :: NtN.HandshakeTr adr ver -> SeverityS
-- severityHandshake (WithMuxBearer _ e) = severityHandshake' e

-- severityHandshake' ::
--      TraceSendRecv (HS.Handshake nt CBOR.Term)
--   -> SeverityS
-- severityHandshake' (TraceSendMsg m) = severityHandshake'' m
-- severityHandshake' (TraceRecvMsg m) = severityHandshake'' m

-- severityHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> SeverityS
-- severityHandshake'' (AnyMessageAndAgency _agency msg) = severityHandshake''' msg

-- severityHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> SeverityS
-- severityHandshake''' HS.MsgProposeVersions {} = Info
-- severityHandshake''' HS.MsgReplyVersions {}   = Info
-- severityHandshake''' HS.MsgAcceptVersion {}   = Info
-- severityHandshake''' HS.MsgRefuse {}          = Info

-- namesForHandshake :: NtN.HandshakeTr adr ver -> [Text]
-- namesForHandshake (WithMuxBearer _ e) = namesForHandshake' e

-- namesForHandshake' ::
--      TraceSendRecv (HS.Handshake nt CBOR.Term)
--   -> [Text]
-- namesForHandshake' (TraceSendMsg m) = "Send" : namesForHandshake'' m
-- namesForHandshake' (TraceRecvMsg m) = "Receive" : namesForHandshake'' m

-- namesForHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> [Text]
-- namesForHandshake'' (AnyMessageAndAgency _agency msg) = namesForHandshake''' msg

-- namesForHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> [Text]
-- namesForHandshake''' HS.MsgProposeVersions {} = Namespace [] "ProposeVersions"]
-- namesForHandshake''' HS.MsgReplyVersions {}   = Namespace [] "ReplyVersions"]
-- namesForHandshake''' HS.MsgAcceptVersion {}   = Namespace [] "AcceptVersion"]
-- namesForHandshake''' HS.MsgRefuse {}          = Namespace [] "Refuse"]


-- docHandshake :: Documented (NtN.HandshakeTr NtN.RemoteAddress ver)
-- docHandshake = addDocumentedNamespace  ["Send"] docHandshake'
--                `addDocs` addDocumentedNamespace  ["Receive"] docHandshake'

-- docHandshake' :: Documented (NtN.HandshakeTr adr ver)
-- docHandshake' = Documented [
--       DocMsg
--         ["ProposeVersions"]
--         []
--         "Propose versions together with version parameters.  It must be\
--         \ encoded to a sorted list.."
--     , DocMsg
--         ["ReplyVersions"]
--         []
--         "`MsgReplyVersions` received as a response to 'MsgProposeVersions'.  It\
--         \ is not supported to explicitly send this message. It can only be\
--         \ received as a copy of 'MsgProposeVersions' in a simultaneous open\
--         \ scenario."
--     , DocMsg
--         ["AcceptVersion"]
--         []
--         "The remote end decides which version to use and sends chosen version.\
--         \The server is allowed to modify version parameters."
--     , DocMsg
--         ["Refuse"]
--         []
--         "It refuses to run any version."
--     ]

-- --------------------------------------------------------------------------------
-- -- LocalHandshake Tracer
-- --------------------------------------------------------------------------------

-- severityLocalHandshake :: NtC.HandshakeTr adr ver -> SeverityS
-- severityLocalHandshake (WithMuxBearer _ e) = severityLocalHandshake' e

-- severityLocalHandshake' ::
--      TraceSendRecv (HS.Handshake nt CBOR.Term)
--   -> SeverityS
-- severityLocalHandshake' (TraceSendMsg m) = severityLocalHandshake'' m
-- severityLocalHandshake' (TraceRecvMsg m) = severityLocalHandshake'' m

-- severityLocalHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> SeverityS
-- severityLocalHandshake'' (AnyMessageAndAgency _agency msg) = severityLocalHandshake''' msg

-- severityLocalHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> SeverityS
-- severityLocalHandshake''' HS.MsgProposeVersions {} = Info
-- severityLocalHandshake''' HS.MsgReplyVersions {}   = Info
-- severityLocalHandshake''' HS.MsgAcceptVersion {}   = Info
-- severityLocalHandshake''' HS.MsgRefuse {}          = Info

-- namesForLocalHandshake :: NtC.HandshakeTr adr ver -> [Text]
-- namesForLocalHandshake (WithMuxBearer _ e) = namesForLocalHandshake' e

-- namesForLocalHandshake' ::
--      TraceSendRecv (HS.Handshake nt CBOR.Term)
--   -> [Text]
-- namesForLocalHandshake' (TraceSendMsg m) = "Send" : namesForLocalHandshake'' m
-- namesForLocalHandshake' (TraceRecvMsg m) = "Receive" : namesForLocalHandshake'' m

-- namesForLocalHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> [Text]
-- namesForLocalHandshake'' (AnyMessageAndAgency _agency msg) = namesForLocalHandshake''' msg

-- namesForLocalHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> [Text]
-- namesForLocalHandshake''' HS.MsgProposeVersions {} = Namespace [] "ProposeVersions"]
-- namesForLocalHandshake''' HS.MsgReplyVersions {}   = Namespace [] "ReplyVersions"]
-- namesForLocalHandshake''' HS.MsgAcceptVersion {}   = Namespace [] "AcceptVersion"]
-- namesForLocalHandshake''' HS.MsgRefuse {}          = Namespace [] "Refuse"]

-- instance LogFormatting (NtC.HandshakeTr NtC.LocalAddress NtC.NodeToClientVersion) where
--   forMachine _dtal (WithMuxBearer b ev) =
--     mconcat [ "kind" .= String "LocalHandshakeTrace"
--              , "bearer" .= show b
--              , "event" .= show ev ]
--   forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
--                                       <> ". " <> showT ev

-- docLocalHandshake :: Documented (NtC.HandshakeTr LocalAddress ver)
-- docLocalHandshake = addDocumentedNamespace  ["Send"] docHandshake'
--                `addDocs` addDocumentedNamespace  ["Receive"] docHandshake'

-- --------------------------------------------------------------------------------
-- -- DiffusionInit Tracer
-- --------------------------------------------------------------------------------

-- severityDiffusionInit :: ND.DiffusionTracer rard ladr -> SeverityS
-- severityDiffusionInit ND.RunServer {}                         = Info
-- severityDiffusionInit ND.RunLocalServer {}                    = Info
-- severityDiffusionInit ND.UsingSystemdSocket {}                = Info
-- severityDiffusionInit ND.CreateSystemdSocketForSnocketPath {} = Info
-- severityDiffusionInit ND.CreatedLocalSocket {}                = Info
-- severityDiffusionInit ND.ConfiguringLocalSocket {}            = Info
-- severityDiffusionInit ND.ListeningLocalSocket {}              = Info
-- severityDiffusionInit ND.LocalSocketUp  {}                    = Info
-- severityDiffusionInit ND.CreatingServerSocket {}              = Info
-- severityDiffusionInit ND.ConfiguringServerSocket {}           = Info
-- severityDiffusionInit ND.ListeningServerSocket {}             = Info
-- severityDiffusionInit ND.ServerSocketUp {}                    = Info
-- severityDiffusionInit ND.UnsupportedLocalSystemdSocket {}     = Warning
-- severityDiffusionInit ND.UnsupportedReadySocketCase {}        = Info
-- severityDiffusionInit ND.DiffusionErrored {}                  = Critical
-- severityDiffusionInit ND.SystemdSocketConfiguration {}        = Warning

-- namesForDiffusionInit  :: ND.DiffusionTracer rard ladr -> [Text]
-- namesForDiffusionInit  ND.RunServer {}                         =
--   ["RunServer"]
-- namesForDiffusionInit  ND.RunLocalServer {}                    =
--   ["RunLocalServer"]
-- namesForDiffusionInit  ND.UsingSystemdSocket {}                =
--   ["UsingSystemdSocket"]
-- namesForDiffusionInit  ND.CreateSystemdSocketForSnocketPath {} =
--   ["CreateSystemdSocketForSnocketPath"]
-- namesForDiffusionInit  ND.CreatedLocalSocket {}                =
--   ["CreatedLocalSocket"]
-- namesForDiffusionInit  ND.ConfiguringLocalSocket {}            =
--   ["ConfiguringLocalSocket"]
-- namesForDiffusionInit  ND.ListeningLocalSocket {}              =
--   ["ListeningLocalSocket"]
-- namesForDiffusionInit  ND.LocalSocketUp  {}                    =
--   ["LocalSocketUp"]
-- namesForDiffusionInit  ND.CreatingServerSocket {}              =
--   ["CreatingServerSocket"]
-- namesForDiffusionInit  ND.ConfiguringServerSocket {}           =
--   ["ConfiguringServerSocket"]
-- namesForDiffusionInit  ND.ListeningServerSocket {}             =
--   ["ListeningServerSocket"]
-- namesForDiffusionInit  ND.ServerSocketUp {}                    =
--   ["ServerSocketUp"]
-- namesForDiffusionInit  ND.UnsupportedLocalSystemdSocket {}     =
--   ["UnsupportedLocalSystemdSocket"]
-- namesForDiffusionInit  ND.UnsupportedReadySocketCase {}        =
--   ["UnsupportedReadySocketCase"]
-- namesForDiffusionInit  ND.DiffusionErrored {}                  =
--   ["DiffusionErrored"]
-- namesForDiffusionInit  ND.SystemdSocketConfiguration {}        =
--   ["SystemdSocketConfiguration"]

-- instance (Show ntnAddr, Show ntcAddr) =>
--   LogFormatting (ND.DiffusionTracer ntnAddr ntcAddr)  where
--   forMachine _dtal (ND.RunServer sockAddr) = mconcat
--     [ "kind" .= String "RunServer"
--     , "socketAddress" .= String (pack (show sockAddr))
--     ]

--   forMachine _dtal (ND.RunLocalServer localAddress) = mconcat
--     [ "kind" .= String "RunLocalServer"
--     , "localAddress" .= String (pack (show localAddress))
--     ]
--   forMachine _dtal (ND.UsingSystemdSocket localAddress) = mconcat
--     [ "kind" .= String "UsingSystemdSocket"
--     , "path" .= String (pack . show $ localAddress)
--     ]

--   forMachine _dtal (ND.CreateSystemdSocketForSnocketPath localAddress) = mconcat
--     [ "kind" .= String "CreateSystemdSocketForSnocketPath"
--     , "path" .= String (pack . show $ localAddress)
--     ]
--   forMachine _dtal (ND.CreatedLocalSocket localAddress) = mconcat
--     [ "kind" .= String "CreatedLocalSocket"
--     , "path" .= String (pack . show $ localAddress)
--     ]
--   forMachine _dtal (ND.ConfiguringLocalSocket localAddress socket) = mconcat
--     [ "kind" .= String "ConfiguringLocalSocket"
--     , "path" .= String (pack . show $ localAddress)
--     , "socket" .= String (pack (show socket))
--     ]
--   forMachine _dtal (ND.ListeningLocalSocket localAddress socket) = mconcat
--     [ "kind" .= String "ListeningLocalSocket"
--     , "path" .=  String (pack . show $ localAddress)
--     , "socket" .= String (pack (show socket))
--     ]
--   forMachine _dtal (ND.LocalSocketUp localAddress fd) = mconcat
--     [ "kind" .= String "LocalSocketUp"
--     , "path" .= String (pack . show $ localAddress)
--     , "socket" .= String (pack (show fd))
--     ]
--   forMachine _dtal (ND.CreatingServerSocket socket) = mconcat
--     [ "kind" .= String "CreatingServerSocket"
--     , "socket" .= String (pack (show socket))
--     ]
--   forMachine _dtal (ND.ListeningServerSocket socket) = mconcat
--     [ "kind" .= String "ListeningServerSocket"
--     , "socket" .= String (pack (show socket))
--     ]
--   forMachine _dtal (ND.ServerSocketUp socket) = mconcat
--     [ "kind" .= String "ServerSocketUp"
--     , "socket" .= String (pack (show socket))
--     ]
--   forMachine _dtal (ND.ConfiguringServerSocket socket) = mconcat
--     [ "kind" .= String "ConfiguringServerSocket"
--     , "socket" .= String (pack (show socket))
--     ]
--   forMachine _dtal (ND.UnsupportedLocalSystemdSocket path) = mconcat
--     [ "kind" .= String "UnsupportedLocalSystemdSocket"
--     , "path" .= String (pack (show path))
--     ]
--   forMachine _dtal ND.UnsupportedReadySocketCase = mconcat
--     [ "kind" .= String "UnsupportedReadySocketCase"
--     ]
--   forMachine _dtal (ND.DiffusionErrored exception) = mconcat
--     [ "kind" .= String "DiffusionErrored"
--     , "path" .= String (pack (show exception))
--     ]
--   forMachine _dtal (ND.SystemdSocketConfiguration config) = mconcat
--     [ "kind" .= String "SystemdSocketConfiguration"
--     , "path" .= String (pack (show config))
--     ]

-- docDiffusionInit :: Documented (ND.DiffusionTracer Socket.SockAddr NtC.LocalAddress)
-- docDiffusionInit =  addDocumentedNamespace  [] docDiffusionInit'

-- docDiffusionInit' :: Documented (ND.DiffusionTracer Socket.SockAddr NtC.LocalAddress)
-- docDiffusionInit' = Documented [
--     DocMsg
--       ["RunServer"]
--       []
--       "RunServer "
--   , DocMsg
--       ["RunLocalServer"]
--       []
--       "RunLocalServer "
--   , DocMsg
--      ["UsingSystemdSocket"]
--       []
--       "UsingSystemdSocket "
--   , DocMsg
--      ["CreateSystemdSocketForSnocketPath"]
--       []
--       "CreateSystemdSocketForSnocketPath "
--   , DocMsg
--       ["CreatedLocalSocket"]
--       []
--       "CreatedLocalSocket "
--   , DocMsg
--       ["ConfiguringLocalSocket"]
--       []
--       "ConfiguringLocalSocket "
--   , DocMsg
--       ["ListeningLocalSocket"]
--       []
--       "ListeningLocalSocket "
--   , DocMsg
--       ["LocalSocketUp"]
--       []
--       "LocalSocketUp "
--   , DocMsg
--       ["CreatingServerSocket"]
--       []
--       "CreatingServerSocket "
--   , DocMsg
--       ["ConfiguringServerSocket"]
--       []
--       "ConfiguringServerSocket "
--   , DocMsg
--       ["ListeningServerSocket"]
--       []
--       "ListeningServerSocket "
--   , DocMsg
--       ["ServerSocketUp"]
--       []
--       "ServerSocketUp "
--   , DocMsg
--       ["UnsupportedLocalSystemdSocket"]
--       []
--       "UnsupportedLocalSystemdSocket "
--   , DocMsg
--       ["UnsupportedReadySocketCase"]
--       []
--       "UnsupportedReadySocketCase "
--   , DocMsg
--       ["DiffusionErrored"]
--       []
--       "DiffusionErrored "
--   ]

-- --------------------------------------------------------------------------------
-- -- LedgerPeers Tracer
-- --------------------------------------------------------------------------------

-- severityLedgerPeers :: TraceLedgerPeers -> SeverityS
-- severityLedgerPeers PickedPeer {}                  = Debug
-- severityLedgerPeers PickedPeers {}                 = Info
-- severityLedgerPeers FetchingNewLedgerState {}      = Info
-- severityLedgerPeers DisabledLedgerPeers {}         = Info
-- severityLedgerPeers TraceUseLedgerAfter {}         = Info
-- severityLedgerPeers WaitingOnRequest {}            = Debug
-- severityLedgerPeers RequestForPeers {}             = Debug
-- severityLedgerPeers ReusingLedgerState {}          = Debug
-- severityLedgerPeers FallingBackToBootstrapPeers {} = Info

-- namesForLedgerPeers :: TraceLedgerPeers -> [Text]
-- namesForLedgerPeers PickedPeer {}                  = Namespace [] "PickedPeer"]
-- namesForLedgerPeers PickedPeers {}                 = Namespace [] "PickedPeers"]
-- namesForLedgerPeers FetchingNewLedgerState {}      = Namespace [] "FetchingNewLedgerState"]
-- namesForLedgerPeers DisabledLedgerPeers {}         = Namespace [] "DisabledLedgerPeers"]
-- namesForLedgerPeers TraceUseLedgerAfter {}         = Namespace [] "TraceUseLedgerAfter"]
-- namesForLedgerPeers WaitingOnRequest {}            = Namespace [] "WaitingOnRequest"]
-- namesForLedgerPeers RequestForPeers {}             = Namespace [] "RequestForPeers"]
-- namesForLedgerPeers ReusingLedgerState {}          = Namespace [] "ReusingLedgerState"]
-- namesForLedgerPeers FallingBackToBootstrapPeers {} = Namespace [] "FallingBackToBootstrapPeers"]


-- instance LogFormatting TraceLedgerPeers where
--   forMachine _dtal (PickedPeer addr _ackStake stake) =
--     mconcat
--       [ "kind" .= String "PickedPeer"
--       , "address" .= show addr
--       , "relativeStake" .= (realToFrac (unPoolStake stake) :: Double)
--       ]
--   forMachine _dtal (PickedPeers (NumberOfPeers n) addrs) =
--     mconcat
--       [ "kind" .= String "PickedPeers"
--       , "desiredCount" .= n
--       , "count" .= length addrs
--       , "addresses" .= show addrs
--       ]
--   forMachine _dtal (FetchingNewLedgerState cnt) =
--     mconcat
--       [ "kind" .= String "FetchingNewLedgerState"
--       , "numberOfPools" .= cnt
--       ]
--   forMachine _dtal DisabledLedgerPeers =
--     mconcat
--       [ "kind" .= String "DisabledLedgerPeers"
--       ]
--   forMachine _dtal (TraceUseLedgerAfter ula) =
--     mconcat
--       [ "kind" .= String "UseLedgerAfter"
--       , "useLedgerAfter" .= UseLedger ula
--       ]
--   forMachine _dtal WaitingOnRequest =
--     mconcat
--       [ "kind" .= String "WaitingOnRequest"
--       ]
--   forMachine _dtal (RequestForPeers (NumberOfPeers np)) =
--     mconcat
--       [ "kind" .= String "RequestForPeers"
--       , "numberOfPeers" .= np
--       ]
--   forMachine _dtal (ReusingLedgerState cnt age) =
--     mconcat
--       [ "kind" .= String "ReusingLedgerState"
--       , "numberOfPools" .= cnt
--       , "ledgerStateAge" .= age
--       ]
--   forMachine _dtal FallingBackToBootstrapPeers =
--     mconcat
--       [ "kind" .= String "FallingBackToBootstrapPeers"
--       ]

-- docLedgerPeers :: Documented TraceLedgerPeers
-- docLedgerPeers =  addDocumentedNamespace  [] docLedgerPeers'

-- docLedgerPeers' :: Documented TraceLedgerPeers
-- docLedgerPeers' = Documented [
--     DocMsg
--       ["PickedPeer"]
--       []
--       "Trace for a peer picked with accumulated and relative stake of its pool."
--   , DocMsg
--       ["PickedPeers"]
--       []
--       "Trace for the number of peers we wanted to pick and the list of peers picked."
--   , DocMsg
--       ["FetchingNewLedgerState"]
--       []
--       "Trace for fetching a new list of peers from the ledger. Int is the number of peers\
--       \ returned."
--   , DocMsg
--       ["DisabledLedgerPeers"]
--       []
--       "Trace for when getting peers from the ledger is disabled, that is DontUseLedger."
--   , DocMsg
--       ["TraceUseLedgerAfter"]
--       []
--       "Trace UseLedgerAfter value."
--   , DocMsg
--       ["WaitingOnRequest"]
--       []
--       ""
--   , DocMsg
--       ["RequestForPeers"]
--       []
--       "RequestForPeers (NumberOfPeers 1)"
--   , DocMsg
--       ["ReusingLedgerState"]
--       []
--       ""
--   , DocMsg
--       ["FallingBackToBootstrapPeers"]
--       []
--       ""
--   ]
