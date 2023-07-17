module Levels
  ( level2,
  )
where

import AccountAccessGraph
  ( CompromisionType (Automatic, NotCompromised, User),
    Node (Node, compromisionType, name, protectedBy),
  )
import Data.Set (fromList)

level2 :: [Node]
level2 =
  [ Node {name = "pw_Bitwarden", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "pw_OTPApp", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "pw_Gmail", protectedBy = fromList [fromList ["Bitwarden"]], compromisionType = NotCompromised},
    Node {name = "otp_Gmail", protectedBy = fromList [fromList ["Finger", "YubiKey"], fromList ["OTPApp"], fromList ["YubiKey", "pw_otp_YubiKey"]], compromisionType = NotCompromised},
    Node {name = "OTPApp_Recovery", protectedBy = fromList [fromList ["USB_Stick"]], compromisionType = NotCompromised},
    Node {name = "OTPApp", protectedBy = fromList [fromList ["Finger", "Phone*"], fromList ["OTPApp_Recovery"], fromList ["Phone*", "pw_OTPApp"]], compromisionType = NotCompromised},
    Node {name = "Bitwarden", protectedBy = fromList [fromList ["Finger", "Phone*"], fromList ["pw_Bitwarden"], fromList ["recoveryKit_Bitwarden"]], compromisionType = NotCompromised},
    Node {name = "Gmail", protectedBy = fromList [fromList ["Meine_lieblings_Frage", "Protonmail"], fromList ["Name_meines_ersten_Haustiers", "Protonmail"], fromList ["otp_Gmail", "pw_Gmail"]], compromisionType = NotCompromised},
    Node {name = "Finger", protectedBy = fromList [fromList ["Glas"]], compromisionType = NotCompromised},
    Node {name = "Phone*", protectedBy = fromList [fromList ["Finger", "Phone"], fromList ["Phone", "code_Phone"]], compromisionType = NotCompromised},
    Node {name = "USB_Stick", protectedBy = fromList [fromList ["Bankschlie\223fach_in_Vechta", "schl\252ssel_Bankschlie\223fach"]], compromisionType = NotCompromised},
    Node {name = "Phone", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "code_Phone", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "YubiKey", protectedBy = fromList [fromList ["Schl\252sselbund"], fromList ["Schreibtisch"]], compromisionType = NotCompromised},
    Node {name = "pw_otp_YubiKey", protectedBy = fromList [fromList ["Bitwarden"]], compromisionType = NotCompromised},
    Node {name = "Schreibtisch", protectedBy = fromList [fromList ["Wohnung"]], compromisionType = NotCompromised},
    Node {name = "Schl\252sselbund", protectedBy = fromList [fromList ["Hosentasche"]], compromisionType = NotCompromised},
    Node {name = "Wohnung", protectedBy = fromList [fromList ["Wohnungsschl\252ssel"]], compromisionType = NotCompromised},
    Node {name = "Hosentasche", protectedBy = fromList [], compromisionType = User},
    Node {name = "Protonmail", protectedBy = fromList [fromList ["Partyheld_Mail"], fromList ["pw_Protonmail"]], compromisionType = NotCompromised},
    Node {name = "pw_Protonmail", protectedBy = fromList [fromList ["Bitwarden"]], compromisionType = NotCompromised},
    Node {name = "Name_meines_ersten_Haustiers", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "Glas", protectedBy = fromList [fromList ["Wohnung"]], compromisionType = NotCompromised},
    Node {name = "Wohnungsschl\252ssel", protectedBy = fromList [fromList ["Schl\252sselbund"]], compromisionType = NotCompromised},
    Node {name = "schl\252ssel_Bankschlie\223fach", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "pw_Farmarama", protectedBy = fromList [fromList ["Bitwarden"], fromList ["Farmarama_Post-it"]], compromisionType = NotCompromised},
    Node {name = "Handvenen", protectedBy = fromList [fromList ["Eistruhe", "Venenscanner"]], compromisionType = NotCompromised},
    Node {name = "Venenscanner", protectedBy = fromList [fromList ["RaspberryPi", "RaspberryPi_Kamera"]], compromisionType = NotCompromised},
    Node {name = "RaspberryPi", protectedBy = fromList [fromList ["Wohnung"]], compromisionType = NotCompromised},
    Node {name = "RaspberryPi_Kamera", protectedBy = fromList [fromList ["Safe"]], compromisionType = NotCompromised},
    Node {name = "otp_Farmarama", protectedBy = fromList [fromList ["Finger", "YubiKey"], fromList ["YubiKey", "pw_otp_YubiKey"]], compromisionType = NotCompromised},
    Node {name = "sicherheitscode_Farmarama", protectedBy = fromList [fromList ["Gmail"]], compromisionType = NotCompromised},
    Node {name = "Farmarama", protectedBy = fromList [fromList ["Handvenen", "otp_Farmarama", "pw_Farmarama", "sicherheitscode_Farmarama"]], compromisionType = NotCompromised},
    Node {name = "Farmarama_Post-it", protectedBy = fromList [fromList ["Bildschirm"]], compromisionType = NotCompromised},
    Node {name = "Bildschirm", protectedBy = fromList [fromList ["Wohnung"]], compromisionType = NotCompromised},
    Node {name = "Meine_lieblings_Frage", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "pw_Partyheld_Mail", protectedBy = fromList [fromList ["pw_Partyheld_Mail_Post-it"]], compromisionType = NotCompromised},
    Node {name = "pw_Partyheld_Mail_Post-it", protectedBy = fromList [fromList ["Safe"]], compromisionType = NotCompromised},
    Node {name = "Safe", protectedBy = fromList [fromList ["Finger", "Safeschl\252ssel"]], compromisionType = NotCompromised},
    Node {name = "Partyheld_Mail", protectedBy = fromList [fromList ["Irisscan", "otp_Partyheld_Mail", "pw_Partyheld_Mail"]], compromisionType = NotCompromised},
    Node {name = "otp_Partyheld_Mail", protectedBy = fromList [fromList ["Finger", "YubiKey"]], compromisionType = NotCompromised},
    Node {name = "Safeschl\252ssel", protectedBy = fromList [fromList ["Wohnung"]], compromisionType = NotCompromised},
    Node {name = "recoveryKit_Bitwarden", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "Irisscan", protectedBy = fromList [fromList ["Auge"], fromList ["Augenarztpraxis_Dr.Keuch"]], compromisionType = NotCompromised},
    Node {name = "Auge", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "Augenarztpraxis_Dr.Keuch", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "Eistruhe", protectedBy = fromList [], compromisionType = NotCompromised},
    Node {name = "Bankschlie\223fach_in_Vechta", protectedBy = fromList [], compromisionType = NotCompromised}
  ]