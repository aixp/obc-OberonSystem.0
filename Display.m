MODULE Display;

	(*
		A. V. Shiryaev, 2012.01
	*)

	TYPE
		Frame* = POINTER TO FrameDesc;	(** Base type of all displayable objects. *)
		FrameDesc* = RECORD (* Objects.ObjDesc *)
			next*, dsc*: Frame;	(** Sibling, child pointers. *)
			X*, Y*, W*, H*: SHORTINT	(** Coordinates. *)
		END;

		FrameMsg* = RECORD (* Objects.ObjMsg *)	(** Base type of messages sent to frames. *)
			F*: Frame; (*target*)	(** Message target, NIL for broadcast. *)
			x*, y*: SHORTINT;	(** Message origin. *)
			res*: SHORTINT;	(** Result code: <0 = error or no response, >=0 response. *)

			stamp*: INTEGER; (* Objects.ObjMsg field *)
		END;

		MsgProc* = PROCEDURE (VAR M: FrameMsg);

	VAR
		Broadcast*: MsgProc;	(** Message broadcast to all frames in the display space. *)

END Display.
