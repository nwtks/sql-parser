module SqlParser.Tests.AccessControlTests

open Xunit
open SqlParser

// 22.1 <direct SQL statement> requires a trailing <semicolon>.
let parse (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parseFails (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

// 12.3 <object name> kind carried by the grant/revoke flat StatementKind cases. The
// routine-designator form is asserted separately through GrantRoutine / RevokeRoutine.
let objectKindFromString (kindName: string) : ObjectKind option =
    match kindName with
    | null -> None
    | "Table" -> Some ObjectKind.Table
    | "Domain" -> Some ObjectKind.Domain
    | "Collation" -> Some ObjectKind.Collation
    | "CharacterSet" -> Some ObjectKind.CharacterSet
    | "Translation" -> Some ObjectKind.Translation
    | "Type" -> Some ObjectKind.Type
    | "Sequence" -> Some ObjectKind.Sequence
    | other -> failwithf "Unknown expected kind %s" other

let grantObjectKind (kind: StatementKind) : ObjectKind option =
    match kind with
    | GrantObject _ -> None
    | GrantTable _ -> Some ObjectKind.Table
    | GrantDomain _ -> Some ObjectKind.Domain
    | GrantCollation _ -> Some ObjectKind.Collation
    | GrantCharacterSet _ -> Some ObjectKind.CharacterSet
    | GrantTranslation _ -> Some ObjectKind.Translation
    | GrantType _ -> Some ObjectKind.Type
    | GrantSequence _ -> Some ObjectKind.Sequence
    | res -> failwithf "Expected a grant privileges case, got %A" res

let grantObjectName (kind: StatementKind) : Expression =
    match kind with
    | GrantObject stmt
    | GrantTable stmt
    | GrantDomain stmt
    | GrantCollation stmt
    | GrantCharacterSet stmt
    | GrantTranslation stmt
    | GrantType stmt
    | GrantSequence stmt -> stmt.Object
    | res -> failwithf "Expected a grant privileges case, got %A" res

let revokeObjectKind (kind: StatementKind) : ObjectKind option =
    match kind with
    | RevokeObject _ -> None
    | RevokeTable _ -> Some ObjectKind.Table
    | RevokeDomain _ -> Some ObjectKind.Domain
    | RevokeCollation _ -> Some ObjectKind.Collation
    | RevokeCharacterSet _ -> Some ObjectKind.CharacterSet
    | RevokeTranslation _ -> Some ObjectKind.Translation
    | RevokeType _ -> Some ObjectKind.Type
    | RevokeSequence _ -> Some ObjectKind.Sequence
    | res -> failwithf "Expected a revoke privileges case, got %A" res

let revokeObjectName (kind: StatementKind) : Expression =
    match kind with
    | RevokeObject stmt
    | RevokeTable stmt
    | RevokeDomain stmt
    | RevokeCollation stmt
    | RevokeCharacterSet stmt
    | RevokeTranslation stmt
    | RevokeType stmt
    | RevokeSequence stmt -> stmt.Object
    | res -> failwithf "Expected a revoke privileges case, got %A" res

// 12.3 <grantor> — the GRANTED BY / WITH ADMIN grantor (deepest asserted parser: pGrantor).
[<Fact>]
let ``GRANTED BY grantor verification`` () =
    match parse "GRANT SELECT ON t1 TO alice GRANTED BY CURRENT_USER" with
    | GrantObject { Grantor = Some Grantor.CurrentUser } -> ()
    | res -> Assert.Fail(sprintf "Expected GRANTED BY CURRENT_USER, got %A" res)

    match parse "GRANT SELECT ON t1 TO alice GRANTED BY CURRENT_ROLE" with
    | GrantObject { Grantor = Some Grantor.CurrentRole } -> ()
    | res -> Assert.Fail(sprintf "Expected GRANTED BY CURRENT_ROLE, got %A" res)

    // Over-permissive extension: an <authorization identifier> is also accepted in the
    // <grantor> position (see docs/trade-off.md).
    match parse "GRANT SELECT ON t1 TO alice GRANTED BY admin_role" with
    | GrantObject { Grantor = Some(Grantor.AuthorizationId { Kind = Identifier "ADMIN_ROLE" }) } -> ()
    | res -> Assert.Fail(sprintf "Expected GRANTED BY authorization identifier, got %A" res)

    // 12.5 <grant role statement> — GRANTED BY <grantor>
    match parse "GRANT role_a TO alice GRANTED BY CURRENT_USER" with
    | GrantRoles([ { Kind = Identifier "ROLE_A" } ],
                 [ Grantee.AuthorizationId { Kind = Identifier "ALICE" } ],
                 false,
                 Some Grantor.CurrentUser) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantRoles GRANTED BY, got %A" res)

[<Fact>]
let ``GRANT verification`` () =
    match parse "GRANT SELECT, INSERT, UPDATE (name) ON TABLE users TO alice, bob WITH GRANT OPTION" with
    | GrantTable stmt ->
        match stmt.Privileges with
        | Privileges.Actions [ PrivilegeAction.Select None
                               PrivilegeAction.Insert None
                               PrivilegeAction.Update(Some [ { Kind = Identifier "NAME" } ]) ] -> ()
        | res -> Assert.Fail(sprintf "Expected SELECT, INSERT, UPDATE privileges, got %A" res)

        match stmt.Object with
        | { Kind = Identifier "USERS" } -> ()
        | res -> Assert.Fail(sprintf "Expected object users, got %A" res)

        match stmt.Grantees with
        | [ Grantee.AuthorizationId { Kind = Identifier "ALICE" }; Grantee.AuthorizationId { Kind = Identifier "BOB" } ] ->
            ()
        | res -> Assert.Fail(sprintf "Expected grantees alice and bob, got %A" res)

        Assert.False(stmt.WithHierarchyOption)
        Assert.True(stmt.WithGrantOption)

        match stmt.Grantor with
        | None -> ()
        | res -> Assert.Fail(sprintf "Expected no grantor, got %A" res)
    | res -> Assert.Fail(sprintf "Expected GrantTable, got %A" res)

    // 12.3 <object name> — the [ TABLE ] kind is optional: a bare name is GrantObject.
    match parse "GRANT ALL PRIVILEGES ON users TO PUBLIC" with
    | GrantObject stmt ->
        match stmt.Privileges with
        | Privileges.AllPrivileges -> ()
        | res -> Assert.Fail(sprintf "Expected ALL PRIVILEGES, got %A" res)

        match stmt.Object with
        | { Kind = Identifier "USERS" } -> ()
        | res -> Assert.Fail(sprintf "Expected object users, got %A" res)

        match stmt.Grantees with
        | [ Grantee.Public ] -> ()
        | res -> Assert.Fail(sprintf "Expected grantee PUBLIC, got %A" res)

        Assert.False(stmt.WithHierarchyOption)
        Assert.False(stmt.WithGrantOption)

        match stmt.Grantor with
        | None -> ()
        | res -> Assert.Fail(sprintf "Expected no grantor, got %A" res)
    | res -> Assert.Fail(sprintf "Expected GrantObject, got %A" res)

    match parse "GRANT SELECT ON users TO alice WITH HIERARCHY OPTION WITH GRANT OPTION GRANTED BY CURRENT_USER" with
    | GrantObject stmt ->
        match stmt.Privileges with
        | Privileges.Actions [ PrivilegeAction.Select None ] -> ()
        | res -> Assert.Fail(sprintf "Expected SELECT privilege, got %A" res)

        Assert.True(stmt.WithHierarchyOption)
        Assert.True(stmt.WithGrantOption)

        match stmt.Grantor with
        | Some Grantor.CurrentUser -> ()
        | res -> Assert.Fail(sprintf "Expected grantor CURRENT_USER, got %A" res)
    | res -> Assert.Fail(sprintf "Expected GrantObject WITH HIERARCHY OPTION, got %A" res)

    // 12.5 <grant role statement>
    match parse "GRANT role_a, role_b TO alice WITH ADMIN OPTION" with
    | GrantRoles([ { Kind = Identifier "ROLE_A" }; { Kind = Identifier "ROLE_B" } ],
                 [ Grantee.AuthorizationId { Kind = Identifier "ALICE" } ],
                 true,
                 None) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantRoles, got %A" res)

[<Theory>]
[<InlineData("GRANT USAGE ON TABLE t1 TO alice", "Table", "T1")>]
[<InlineData("GRANT USAGE ON DOMAIN d1 TO alice", "Domain", "D1")>]
[<InlineData("GRANT USAGE ON COLLATION c1 TO alice", "Collation", "C1")>]
[<InlineData("GRANT USAGE ON CHARACTER SET cs1 TO alice", "CharacterSet", "CS1")>]
[<InlineData("GRANT USAGE ON TRANSLATION tr1 TO alice", "Translation", "TR1")>]
[<InlineData("GRANT USAGE ON TYPE ty1 TO alice", "Type", "TY1")>]
[<InlineData("GRANT USAGE ON SEQUENCE s1 TO alice", "Sequence", "S1")>]
[<InlineData("GRANT USAGE ON t1 TO alice", null, "T1")>]
let ``GRANT object kind verification`` sql expectedKind expectedName =
    let expected = objectKindFromString expectedKind
    let stmt = parse sql

    Assert.Equal(expected, grantObjectKind stmt)

    match grantObjectName stmt with
    | { Kind = Identifier name } -> Assert.Equal(expectedName, name)
    | res -> Assert.Fail(sprintf "Expected an identifier object name, got %A" res)

[<Fact>]
let ``GRANT UNDER privilege verification`` () =
    match parse "GRANT UNDER ON TABLE users TO alice" with
    | GrantTable stmt ->
        match stmt.Privileges with
        | Privileges.Actions [ PrivilegeAction.Under ] -> ()
        | res -> Assert.Fail(sprintf "Expected UNDER privilege, got %A" res)

        match stmt.Object with
        | { Kind = Identifier "USERS" } -> ()
        | res -> Assert.Fail(sprintf "Expected object users, got %A" res)

        match stmt.Grantees with
        | [ Grantee.AuthorizationId { Kind = Identifier "ALICE" } ] -> ()
        | res -> Assert.Fail(sprintf "Expected grantee alice, got %A" res)
    | res -> Assert.Fail(sprintf "Expected GrantTable, got %A" res)

[<Fact>]
let ``GRANT EXECUTE ON routine verification`` () =
    // 12.3 <object name> — the <specific routine designator> alternative keeps the
    // 10.6 <routine type> in the AST.
    match parse "GRANT EXECUTE ON FUNCTION add TO alice" with
    | GrantRoutine(RoutineType.Function, stmt) ->
        match stmt.Object with
        | { Kind = Identifier "ADD" } -> ()
        | res -> Assert.Fail(sprintf "Expected object add, got %A" res)

        match stmt.Grantees with
        | [ Grantee.AuthorizationId { Kind = Identifier "ALICE" } ] -> ()
        | res -> Assert.Fail(sprintf "Expected grantee alice, got %A" res)
    | res -> Assert.Fail(sprintf "Expected GrantRoutine FUNCTION, got %A" res)

    match parse "GRANT SELECT ON PROCEDURE p TO bob" with
    | GrantRoutine(RoutineType.Procedure, stmt) ->
        match stmt.Object with
        | { Kind = Identifier "P" } -> ()
        | res -> Assert.Fail(sprintf "Expected object p, got %A" res)
    | res -> Assert.Fail(sprintf "Expected GrantRoutine PROCEDURE, got %A" res)

    // <specific routine designator> in the SELECT <privilege method list>
    match parse "GRANT SELECT (SPECIFIC FUNCTION f) ON TYPE my_type TO alice" with
    | GrantType stmt ->
        match stmt.Privileges with
        | Privileges.Actions [ PrivilegeAction.Select(Some(PrivilegeMethods [ designator ])) ] ->
            Assert.True(designator.IsSpecific)
            Assert.Equal(Some RoutineType.Function, designator.RoutineType)
            Assert.Equal(Identifier "F", designator.Name.Kind)

            match stmt.Object with
            | { Kind = Identifier "MY_TYPE" } -> ()
            | res -> Assert.Fail(sprintf "Expected object my_type, got %A" res)
        | res -> Assert.Fail(sprintf "Expected SELECT (method list), got %A" res)
    | res -> Assert.Fail(sprintf "Expected GrantType, got %A" res)

[<Fact>]
let ``CREATE ROLE verification`` () =
    // 12.4 <role definition> ::= CREATE ROLE <role name> [ WITH ADMIN <grantor> ]
    match parse "CREATE ROLE admin" with
    | CreateRole({ Kind = Identifier "ADMIN" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateRole, got %A" res)

    match parse "CREATE ROLE analyst WITH ADMIN CURRENT_USER" with
    | CreateRole({ Kind = Identifier "ANALYST" }, Some Grantor.CurrentUser) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateRole WITH ADMIN, got %A" res)

[<Fact>]
let ``REVOKE verification`` () =
    match parse "REVOKE SELECT, DELETE ON users FROM alice CASCADE" with
    | RevokeObject stmt ->
        match stmt.Privileges with
        | Privileges.Actions [ PrivilegeAction.Select None; PrivilegeAction.Delete ] -> ()
        | res -> Assert.Fail(sprintf "Expected SELECT, DELETE privileges, got %A" res)

        match stmt.Object with
        | { Kind = Identifier "USERS" } -> ()
        | res -> Assert.Fail(sprintf "Expected object users, got %A" res)

        match stmt.Grantees with
        | [ Grantee.AuthorizationId { Kind = Identifier "ALICE" } ] -> ()
        | res -> Assert.Fail(sprintf "Expected grantee alice, got %A" res)

        match stmt.Option with
        | NoOption -> ()
        | res -> Assert.Fail(sprintf "Expected no revoke option, got %A" res)

        match stmt.Grantor with
        | None -> ()
        | res -> Assert.Fail(sprintf "Expected no grantor, got %A" res)

        Assert.True(stmt.DropBehavior)
    | res -> Assert.Fail(sprintf "Expected RevokeObject, got %A" res)

    match parse "REVOKE GRANT OPTION FOR SELECT ON users FROM alice RESTRICT" with
    | RevokeObject stmt ->
        match stmt.Option with
        | GrantOptionFor -> ()
        | res -> Assert.Fail(sprintf "Expected GRANT OPTION FOR, got %A" res)

        Assert.False(stmt.DropBehavior)
    | res -> Assert.Fail(sprintf "Expected RevokeObject GRANT OPTION FOR, got %A" res)

    match parse "REVOKE HIERARCHY OPTION FOR SELECT ON users FROM alice CASCADE" with
    | RevokeObject stmt ->
        match stmt.Option with
        | HierarchyOptionFor -> ()
        | res -> Assert.Fail(sprintf "Expected HIERARCHY OPTION FOR, got %A" res)

        Assert.True(stmt.DropBehavior)
    | res -> Assert.Fail(sprintf "Expected RevokeObject HIERARCHY OPTION FOR, got %A" res)

    // 12.7 <revoke privilege statement> — the <specific routine designator> form
    match parse "REVOKE EXECUTE ON FUNCTION add FROM alice CASCADE" with
    | RevokeRoutine(RoutineType.Function, stmt) ->
        match stmt.Object with
        | { Kind = Identifier "ADD" } -> ()
        | res -> Assert.Fail(sprintf "Expected object add, got %A" res)

        Assert.True(stmt.DropBehavior)
    | res -> Assert.Fail(sprintf "Expected RevokeRoutine FUNCTION, got %A" res)

    // 12.7 <revoke role statement>
    match parse "REVOKE role_a FROM alice CASCADE" with
    | RevokeRoles([ { Kind = Identifier "ROLE_A" } ],
                  [ Grantee.AuthorizationId { Kind = Identifier "ALICE" } ],
                  false,
                  None,
                  true) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokeRoles, got %A" res)

    match parse "REVOKE ADMIN OPTION FOR role_a FROM alice CASCADE" with
    | RevokeRoles([ { Kind = Identifier "ROLE_A" } ],
                  [ Grantee.AuthorizationId { Kind = Identifier "ALICE" } ],
                  true,
                  None,
                  true) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokeRoles ADMIN OPTION FOR, got %A" res)

    match parse "REVOKE role_a FROM alice GRANTED BY CURRENT_ROLE CASCADE" with
    | RevokeRoles([ { Kind = Identifier "ROLE_A" } ],
                  [ Grantee.AuthorizationId { Kind = Identifier "ALICE" } ],
                  false,
                  Some Grantor.CurrentRole,
                  true) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokeRoles GRANTED BY, got %A" res)

[<Theory>]
[<InlineData("REVOKE SELECT ON TABLE t1 FROM alice CASCADE", "Table", "T1")>]
[<InlineData("REVOKE SELECT ON DOMAIN d1 FROM alice CASCADE", "Domain", "D1")>]
[<InlineData("REVOKE SELECT ON COLLATION c1 FROM alice RESTRICT", "Collation", "C1")>]
[<InlineData("REVOKE SELECT ON CHARACTER SET cs1 FROM alice CASCADE", "CharacterSet", "CS1")>]
[<InlineData("REVOKE SELECT ON TRANSLATION tr1 FROM alice CASCADE", "Translation", "TR1")>]
[<InlineData("REVOKE SELECT ON TYPE ty1 FROM alice CASCADE", "Type", "TY1")>]
[<InlineData("REVOKE SELECT ON SEQUENCE s1 FROM alice CASCADE", "Sequence", "S1")>]
[<InlineData("REVOKE SELECT ON t1 FROM alice CASCADE", null, "T1")>]
let ``REVOKE object kind verification`` sql expectedKind expectedName =
    let expected = objectKindFromString expectedKind
    let stmt = parse sql

    Assert.Equal(expected, revokeObjectKind stmt)

    match revokeObjectName stmt with
    | { Kind = Identifier name } -> Assert.Equal(expectedName, name)
    | res -> Assert.Fail(sprintf "Expected an identifier object name, got %A" res)

[<Fact>]
let ``REVOKE requires drop behavior`` () =
    parseFails "REVOKE SELECT ON users FROM alice"
    parseFails "REVOKE role_a FROM alice"

[<Fact>]
let ``GRANT to PUBLIC verification`` () =
    match parse "GRANT SELECT ON users TO PUBLIC" with
    | GrantObject stmt ->
        match stmt.Grantees with
        | [ Grantee.Public ] -> ()
        | res -> Assert.Fail(sprintf "Expected grantee PUBLIC, got %A" res)
    | res -> Assert.Fail(sprintf "Expected GrantObject, got %A" res)

[<Fact>]
let ``REVOKE from PUBLIC verification`` () =
    match parse "REVOKE SELECT ON users FROM PUBLIC CASCADE" with
    | RevokeObject stmt ->
        match stmt.Grantees with
        | [ Grantee.Public ] -> ()
        | res -> Assert.Fail(sprintf "Expected grantee PUBLIC, got %A" res)
    | res -> Assert.Fail(sprintf "Expected RevokeObject, got %A" res)

[<Fact>]
let ``GRANT with invalid grantee is rejected`` () =
    // String literals are not valid grantees
    parseFails "GRANT SELECT ON users TO 'alice'"
    // Arithmetic expressions are not valid grantees
    parseFails "GRANT SELECT ON users TO a + b"
