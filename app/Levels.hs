module Levels
  ( tutorial1,
    tutorial2,
    tutorial3,
    tutorial4,
  )
where

import AccountAccessGraph
  ( CompromisionType (..),
    Node (Node, compromisionType, name, protectedBy),
  )
import Data.Set (fromList)

tutorial1 :: [Node]
tutorial1 = [Node {name = "pw_Mail", protectedBy = fromList [], compromisionType = User}, Node {name = "Mail", protectedBy = fromList [fromList ["pw_Mail"]], compromisionType = NotCompromised}]

tutorial2 :: [Node]
tutorial2 = [Node {name = "pw_Mail", protectedBy = fromList [], compromisionType = User}, Node {name = "Mail", protectedBy = fromList [fromList ["pw_Mail"]], compromisionType = User}, Node {name = "Netflix", protectedBy = fromList [fromList ["otp_Netflix", "pw_Netflix"]], compromisionType = NotCompromised}, Node {name = "pw_Netflix", protectedBy = fromList [], compromisionType = OpenQuest}, Node {name = "otp_Netflix", protectedBy = fromList [], compromisionType = User}]

tutorial3 :: [Node]
tutorial3 = [Node {name = "Mail", protectedBy = fromList [fromList ["pw_Mail"]], compromisionType = User},Node {name = "pw_Mail", protectedBy = fromList [], compromisionType = User},Node {name = "Netflix", protectedBy = fromList [fromList ["otp_Netflix","pw_Netflix"]], compromisionType = User},Node {name = "pw_Netflix", protectedBy = fromList [], compromisionType = User},Node {name = "otp_Netflix", protectedBy = fromList [], compromisionType = User},Node {name = "Amazon", protectedBy = fromList [fromList ["Mail"],fromList ["otp_Amazon","pw_Amazon"]], compromisionType = NotCompromised},Node {name = "otp_Amazon", protectedBy = fromList [], compromisionType = User},Node {name = "pw_Amazon", protectedBy = fromList [], compromisionType = NotCompromised}]

tutorial4 :: [Node]
tutorial4 = [Node {name = "pw_Bitwarden", protectedBy = fromList [], compromisionType = User}, Node {name = "pw_OTPApp", protectedBy = fromList [], compromisionType = NotCompromised}, Node {name = "pw_Gmail", protectedBy = fromList [fromList ["Bitwarden"]], compromisionType = NotCompromised}, Node {name = "otp_Gmail", protectedBy = fromList [fromList ["OTPApp"], fromList ["YubiKey"]], compromisionType = NotCompromised}, Node {name = "OTPApp_Recovery", protectedBy = fromList [fromList ["USB_Stick"]], compromisionType = NotCompromised}, Node {name = "OTPApp", protectedBy = fromList [fromList ["Finger", "Phone"], fromList ["OTPApp_Recovery"], fromList ["Phone", "pw_OTPApp"]], compromisionType = NotCompromised}, Node {name = "Bitwarden", protectedBy = fromList [fromList ["Finger", "Phone"], fromList ["pw_Bitwarden"]], compromisionType = NotCompromised}, Node {name = "Gmail", protectedBy = fromList [fromList ["otp_Gmail", "pw_Gmail"]], compromisionType = NotCompromised}, Node {name = "Finger", protectedBy = fromList [], compromisionType = NotCompromised}, Node {name = "YubiKey", protectedBy = fromList [], compromisionType = NotCompromised}, Node {name = "Phone", protectedBy = fromList [], compromisionType = User}, Node {name = "USB_Stick", protectedBy = fromList [], compromisionType = User}, Node {name = "Hetzner", protectedBy = fromList [fromList ["Gmail"]], compromisionType = NotCompromised}]