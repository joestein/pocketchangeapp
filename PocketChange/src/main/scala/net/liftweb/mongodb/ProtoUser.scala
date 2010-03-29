package net.liftweb.mongodb

import _root_.java.util.{Locale, TimeZone}
import _root_.scala.xml.{NodeSeq, Node, Group, Text, Elem}
import _root_.scala.xml.transform._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.net.liftweb.util.Helpers._
import _root_.net.liftweb.util._
import _root_.net.liftweb.util.Mailer._
import _root_.net.liftweb.http._
import js._
import JsCmds._
import S._

import _root_.com.mongodb.ObjectId
import _root_.com.osinka.mongodb._
import _root_.com.osinka.mongodb.shape._
import Preamble._

object ProtoUser {
    val uniqueIdLen = 32
    val defaultPassword = "*"
    val blankPw = "*******"

    def genUniqueId = randomString(uniqueIdLen)
}

trait MegaProtoUser extends MongoObject with EasyID {
    var firstName: String = ""
    var lastName: String = ""
    var email: String = ""
    val password = FatLazy(ProtoUser.defaultPassword)
    var uniqueId: String = ProtoUser.genUniqueId

    def userIdAsString: String = id

    var superUser: Boolean = false
    var validated: Boolean = false
    var locale: Locale = Locale.getDefault
    var timezone: TimeZone = TimeZone.getDefault

    def niceName: String = (firstName, lastName, email) match {
        case (f, l, e) if f.length > 1 && l.length > 1 => f+" "+l+" ("+e+")"
        case (f, _, e) if f.length > 1 => f+" ("+e+")"
        case (_, l, e) if l.length > 1 => l+" ("+e+")"
        case (_, _, e) => e
    }

    def shortName: String = (firstName, lastName) match {
        case (f, l) if f.length > 1 && l.length > 1 => f+" "+l
        case (f, _) if f.length > 1 => f
        case (_, l) if l.length > 1 => l
        case _ => email
    }

    def niceNameWEmailLink = <a href={"mailto:"+email}>{niceName}</a>

    def resetUniqueId {
        uniqueId = ProtoUser.genUniqueId
    }

    val salt_i = FatLazy(randomString(16))
    var invalidPw = false
    var invalidMsg = ""

    def passwordMatch_?(toMatch: String): Boolean =
        hash("{"+toMatch+"} salt={"+salt_i.get+"}") == password.get
    
    def real_set_passwd(value: String) {
        password() = value match {
            case "*" | null | ProtoUser.blankPw if (value.length < 3) => {invalidPw = true ; invalidMsg = S.??("password.must.be.set") ; "*"}
            case ProtoUser.blankPw => {return "*"}
            case _ if (value.length > 4) => {invalidPw = false; hash("{"+value+"} salt={"+salt_i.get+"}")}
            case _ => {invalidPw = true ; invalidMsg = S.??("password.too.short"); "*"}
        }
    }
}

trait MetaMegaProtoUser[ModelType <: MegaProtoUser] extends MongoObjectShape[ModelType] with MetaMapper[ModelType] { userShape =>

    def create: ModelType

    object firstName extends ScalarField[String]("firstName", _.firstName, Some((x: ModelType, v: String) => x.firstName = v)) with MappedString {
        override def displayName = firstNameDisplayName
        override val fieldId = Some(Text("txtFirstName"))
    }
    
    def firstNameDisplayName = ??("First Name")

    object lastName extends ScalarField[String]("lastName", _.lastName, Some((x: ModelType, v: String) => x.lastName = v)) with MappedString {
        override def displayName = lastNameDisplayName
        override val fieldId = Some(Text("txtLastName"))
    }

    def lastNameDisplayName = ??("Last Name")

    object email extends ScalarField[String]("email", _.email.toLowerCase.trim, Some((x: ModelType, v: String) => x.email = v)) with MappedString {
        override def displayName = emailDisplayName
        override val fieldId = Some(Text("txtEmail"))
        
        val emailPattern = java.util.regex.Pattern.compile("^[a-z0-9._%-]+@(?:[a-z0-9-]+\\.)+[a-z]{2,4}$")
        def validEmailAddr_?(email: String): Boolean = emailPattern.matcher(email).matches

        def valUnique(msg: => String)(value: String): List[FieldError] =
            userShape.all {this is_== value}.toList map(x =>FieldError(this, Text(msg)))

        override def validations = valUnique(S.??("unique.email.address")) _ :: super.validations

        override def validate(obj: ModelType) =
            (if (emailPattern.matcher(obj.email).matches) Nil else List(FieldError(this, Text(S.??("invalid.email.address"))))) ::: super.validate(obj)
    }


    def emailDisplayName = ??("Email")

    object password extends ScalarField[String]("password", _.password.get, Some((x: ModelType, v: String) => x.password() = v)) with MappedString {
        override def displayName = passwordDisplayName
        
        override def validate(obj: ModelType): List[FieldError] = {
            Log.debug("validate password: "+obj.invalidPw+"/"+obj.invalidMsg+" <- "+obj.password.get)
            if (!obj.invalidPw && obj.password.get != "*") Nil
            else if (obj.invalidPw) List(FieldError(this, Text(obj.invalidMsg)))
            else List(FieldError(this, Text(S.??("password.must.set"))))
        }

        override def _toForm(obj: ModelType): Box[NodeSeq] =
            S.fmapFunc({s: List[String] => setFromAny(obj, s)}){funcName =>
                Full(<span><input id={fieldId} type='password' name={funcName}
                    value={rep.get(obj) map { _.toString } getOrElse ""}/>&nbsp;{S.??("repeat")}&nbsp;<input
                    type='password' name={funcName} lift:gc={funcName}
                    value={rep.get(obj) map { _.toString } getOrElse ""}/></span>)
            }

        override def setFromAny(obj: ModelType, f: Any): Boolean = f match {
            case a : Array[String] if (a.length == 2 && a(0) == a(1)) =>
                obj real_set_passwd a(0)
                true
            case l : List[String] if (l.length == 2 && l.head == l(1)) =>
                obj real_set_passwd l.head
                true
            case _ =>
                obj.invalidPw = true
                obj.invalidMsg = S.??("passwords.do.not.match")
                false
        }
    }

    def passwordDisplayName = ??("Password")

    lazy val uniqueId = Field.scalar("uniqueId", _.uniqueId, (x: ModelType, v: String) => x.uniqueId = v)
    lazy val validated = Field.scalar("validated", _.validated, (x: ModelType, v: Boolean) => x.validated = v)
    lazy val salt = Field.scalar("salt", _.salt_i.get, (x: ModelType, v: String) => x.salt_i() = v)

    object locale extends MongoScalar[String] with ScalarContent[String] with MappedString {
        override def displayName = localeDisplayName
        override val fieldId = Some(Text("txtLocale"))
        override val mongoFieldName = "locale"
        override val rep = new FieldRep[String] {
            override def get[A1>:String](x: ModelType): Option[A1] = Some(x.locale.toString)
            override def put[A2<:String](x: ModelType)(a: Option[A2]) {
                x.locale = (for {l <- a.toSeq
                                 x <- Locale.getAvailableLocales
                                 if x.toString == l} yield x).firstOption getOrElse Locale.getDefault
            }
        }
    }

    def localeDisplayName = ??("Locale")

    object timezone extends MongoScalar[String] with ScalarContent[String] with MappedString {
        override def displayName = timezoneDisplayName
        override val fieldId = Some(Text("txtTimeZone"))
        override val mongoFieldName = "tz"
        override val rep = new FieldRep[String] {
            override def get[A1>:String](x: ModelType): Option[A1] = Some(x.timezone.getID)
            override def put[A2<:String](x: ModelType)(a: Option[A2]) {
                x.timezone = a flatMap {tz => Helpers.tryo(TimeZone getTimeZone tz)} getOrElse TimeZone.getDefault
            }
        }
    }

    def timezoneDisplayName = ??("Time Zone")

    override def * : List[MongoField[_]] = List(validated, uniqueId, salt, firstName, lastName, email, password, locale, timezone)

    lazy val timeZoneList = TimeZone.getAvailableIDs.toList.
        filter(!_.startsWith("SystemV/")).
        filter(!_.startsWith("Etc/")).filter(_.length > 3).
        sort(_ < _).map(tz => (tz, tz))

    def signupFields = firstName :: lastName :: email :: locale :: timezone :: password :: Nil

    def screenWrap: Box[Node] = Empty

    val basePath: List[String] = "user_mgt" :: Nil
    def signUpSuffix = "sign_up"
    lazy val signUpPath = thePath(signUpSuffix)
    def loginSuffix = "login"
    lazy val loginPath = thePath(loginSuffix)
    def lostPasswordSuffix = "lost_password"
    lazy val lostPasswordPath = thePath(lostPasswordSuffix)
    def passwordResetSuffix = "reset_password"
    lazy val passwordResetPath = thePath(passwordResetSuffix)
    def changePasswordSuffix = "change_password"
    lazy val changePasswordPath = thePath(changePasswordSuffix)
    def logoutSuffix = "logout"
    lazy val logoutPath = thePath(logoutSuffix)
    def editSuffix = "edit"
    lazy val editPath = thePath(editSuffix)
    def validateUserSuffix = "validate_user"
    lazy val validateUserPath = thePath(validateUserSuffix)

    def homePage = "/"

    case class MenuItem(name: String, path: List[String], loggedIn: Boolean) {
        lazy val endOfPath = path.last
        lazy val pathStr: String = path.mkString("/", "/", "")
        lazy val display = name match {
            case null | "" => false
            case _ => true
        }
    }

    def thePath(end: String): List[String] = basePath ::: List(end)

    /**
     * Return the URL of the "login" page
     */
    def loginPageURL = loginPath.mkString("/","/", "")

    def notLoggedIn_? = !loggedIn_?

    lazy val testLogginIn = If(loggedIn_? _, S.??("must.be.logged.in")) ;

    lazy val testSuperUser = If(superUser_? _, S.??("must.be.super.user"))

    def superUser_? : Boolean = currentUser.map(_.superUser) openOr false

    /**
     * The menu item for login (make this "Empty" to disable)
     */
    def loginMenuLoc: Box[Menu] = {
        Full(Menu(Loc("Login", loginPath, S.??("login"),
                      If(notLoggedIn_? _, S.??("already.logged.in")),
                      Template(() => wrapIt(login)))))
    }

    /**
     * The menu item for logout (make this "Empty" to disable)
     */
    def logoutMenuLoc: Box[Menu] =
        Full(Menu(Loc("Logout", logoutPath, S.??("logout"),
                      Template(() => wrapIt(logout)),
                      testLogginIn)))

    /**
     * The menu item for creating the user/sign up (make this "Empty" to disable)
     */
    def createUserMenuLoc: Box[Menu] =
        Full(Menu(Loc("CreateUser", signUpPath,
                      S.??("sign.up"),
                      Template(() => wrapIt(signupFunc.map(_()) openOr signup)),
                      If(notLoggedIn_? _, S.??("logout.first")))))

    /**
     * The menu item for lost password (make this "Empty" to disable)
     */
    def lostPasswordMenuLoc: Box[Menu] =
    Full(Menu(Loc("LostPassword", lostPasswordPath,
                  S.??("lost.password"),
                  Template(() => wrapIt(lostPassword)),
                  If(notLoggedIn_? _, S.??("logout.first"))))) // not logged in

    /**
     * The menu item for resetting the password (make this "Empty" to disable)
     */
    def resetPasswordMenuLoc: Box[Menu] =
        Full(Menu(Loc("ResetPassword", (passwordResetPath, true),
                      S.??("reset.password"), Hidden,
                      Template(() => wrapIt(passwordReset(snarfLastItem))),
                      If(notLoggedIn_? _,
                         S.??("logout.first"))))) //not Logged in

    /**
     * The menu item for editing the user (make this "Empty" to disable)
     */
    def editUserMenuLoc: Box[Menu] =
        Full(Menu(Loc("EditUser", editPath, S.??("edit.user"),
                      Template(() => wrapIt(editFunc.map(_()) openOr edit)),
                      testLogginIn)))

    /**
     * The menu item for changing password (make this "Empty" to disable)
     */
    def changePasswordMenuLoc: Box[Menu] =
        Full(Menu(Loc("ChangePassword", changePasswordPath,
                      S.??("change.password"),
                      Template(() => wrapIt(changePassword)),
                      testLogginIn)))

    /**
     * The menu item for validating a user (make this "Empty" to disable)
     */
    def validateUserMenuLoc: Box[Menu] =
        Full(Menu(Loc("ValidateUser", (validateUserPath, true),
                      S.??("validate.user"), Hidden,
                      Template(() => wrapIt(validateUser(snarfLastItem))),
                      If(notLoggedIn_? _, S.??("logout.first")))))

    lazy val sitemap: List[Menu] =
        List(loginMenuLoc, logoutMenuLoc, createUserMenuLoc,
             lostPasswordMenuLoc, resetPasswordMenuLoc,
             editUserMenuLoc, changePasswordMenuLoc,
             validateUserMenuLoc).flatten(a => a)


    def skipEmailValidation = false

    def userMenu: List[Node] = {
        val li = loggedIn_?
        ItemList.
        filter(i => i.display && i.loggedIn == li).
        map(i => (<a href={i.pathStr}>{i.name}</a>))
    }

    protected def snarfLastItem: String =
        (for (r <- S.request) yield r.path.wholePath.last) openOr ""

    lazy val ItemList: List[MenuItem] =
        List(MenuItem(S.??("sign.up"), signUpPath, false),
             MenuItem(S.??("log.in"), loginPath, false),
             MenuItem(S.??("lost.password"), lostPasswordPath, false),
             MenuItem("", passwordResetPath, false),
             MenuItem(S.??("change.password"), changePasswordPath, true),
             MenuItem(S.??("log.out"), logoutPath, true),
             MenuItem(S.??("edit.profile"), editPath, true),
             MenuItem("", validateUserPath, false))

    var onLogIn: List[ModelType => Unit] = Nil

    var onLogOut: List[Box[ModelType] => Unit] = Nil

    def loggedIn_? : Boolean = currentUserId.isDefined

    def logUserIdIn(id: String) {
        curUser.remove()
        curUserId(Full(id))
    }

    def logUserIn(who: ModelType) {
        curUser.remove()
        curUserId(Full(who.id))
        onLogIn.foreach(_(who))
    }

    def logoutCurrentUser = logUserOut()

    def logUserOut() {
        onLogOut.foreach(_(curUser))
        curUserId.remove()
        curUser.remove()
        S.request.foreach(_.request.getSession.invalidate)
    }

    private object curUserId extends SessionVar[Box[String]](Empty)

    def currentUserId: Box[String] = curUserId.is

    private object curUser extends RequestVar[Box[ModelType]](currentUserId.flatMap(id =>
            one( this where {oid is_== new ObjectId(id)} )
        ) )

    def currentUser: Box[ModelType] = curUser.is

    def signupXhtml(user: ModelType) = {
        (<form method="post" action={S.uri}><table>
            <tr><td colspan="2">Sign Up</td></tr>
            {localForm(user, false)}
            <tr><td>&nbsp;</td><td><user:submit/></td></tr>
         </table></form>)
    }

    def signupMailBody(user: ModelType, validationLink: String) = {
        (<html>
            <head>
              <title>Sign Up Confirmation</title>
            </head>
            <body>
              <p>Dear {user.firstName},
                <br/>
                <br/>
                Click on this link to complete signup
                <br/><a href={validationLink}>{validationLink}</a>
                <br/>
                <br/>
                Thanks
              </p>
            </body>
         </html>)
    }

    def signupMailSubject = S.??("sign.up.confirmation")

    def sendValidationEmail(user: ModelType) {
        val resetLink = S.hostAndPath+"/"+validateUserPath.mkString("/")+"/"+user.uniqueId

        val email: String = user.email

        val msgXml = signupMailBody(user, resetLink)

        Mailer.sendMail(From(emailFrom),Subject(signupMailSubject),
                        (To(user.email) :: xmlToMailBodyType(msgXml) ::
                         (bccEmail.toList.map(BCC(_)))) :_* )
    }

    protected object signupFunc extends RequestVar[Box[() => NodeSeq]](Empty)

    def signup = {
        val theUser: ModelType = create
        val theName = signUpPath.mkString("")

        def testSignup() {
            Log.debug("testSignup called")
            validate(theUser) match {
                case Nil =>
                    theUser.validated = skipEmailValidation
                    theUser.resetUniqueId
                    save(theUser)
                    if (!skipEmailValidation) {
                        sendValidationEmail(theUser)
                        S.notice(S.??("sign.up.message"))
                    } else {
                        S.notice(S.??("welcome"))
                        logUserIn(theUser)
                    }

                    S.redirectTo(homePage)

                case xs =>
                    S.error(xs)
                    signupFunc(Full(innerSignup _))
            }
        }

        def innerSignup = bind("user",
                               signupXhtml(theUser),
                               "submit" -> SHtml.submit(S.??("sign.up"), testSignup _))

        innerSignup
    }

    def emailFrom = "noreply@"+S.hostName

    def bccEmail: Box[String] = Empty

    def testLoggedIn(page: String): Boolean =
        ItemList.filter(_.endOfPath == page) match {
            case x :: xs if x.loggedIn == loggedIn_? => true
            case _ => false
        }

    def validateUser(id: String): NodeSeq = one( this where {uniqueId is_== id} ) match {
        case Full(user) if !user.validated =>
            user.validated = true
            user.resetUniqueId
            save(user)
            S.notice(S.??("account.validated"))
            logUserIn(user)
            S.redirectTo(homePage)

        case _ => S.error(S.??("invalid.validation.link")); S.redirectTo(homePage)
    }

    def loginXhtml = {
        (<form method="post" action={S.uri}><table><tr><td
                  colspan="2">{S.??("log.in")}</td></tr>
              <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
              <tr><td>{S.??("password")}</td><td><user:password /></td></tr>
              <tr><td><a href={lostPasswordPath.mkString("/", "/", "")}
                    >{S.??("recover.password")}</a></td><td><user:submit /></td></tr></table>
         </form>)
    }

    def login = {
        if (S.post_?) {
            S.param("username").
            flatMap(username => one( this where {email is_== username} )) match {
                case Full(user) if user.validated &&
                    user.passwordMatch_?(S.param("password").openOr("*")) =>
                    logUserIn(user); S.notice(S.??("logged.in")); S.redirectTo(homePage)

                case Full(user) if !user.validated =>
                    S.error(S.??("account.validation.error"))

                case _ => S.error(S.??("invalid.credentials"))
            }
        }

        bind("user", loginXhtml,
             "email" -> (FocusOnLoad(<input type="text" name="username"/>)),
             "password" -> (<input type="password" name="password"/>),
             "submit" -> (<input type="submit" value={S.??("log.in")}/>))
    }

    def lostPasswordXhtml = {
        (<form method="post" action={S.uri}>
         <table><tr><td
              colspan="2">{S.??("enter.email")}</td></tr>
           <tr><td>{S.??("email.address")}</td><td><user:email /></td></tr>
           <tr><td>&nbsp;</td><td><user:submit /></td></tr>
         </table>
         </form>)
    }

    def passwordResetMailBody(user: ModelType, resetLink: String) = {
        (<html>
          <head>
            <title>{S.??("reset.password.confirmation")}</title>
          </head>
          <body>
            <p>{S.??("dear")} {user.firstName},
              <br/>
              <br/>
              {S.??("click.reset.link")}
              <br/><a href={resetLink}>{resetLink}</a>
              <br/>
              <br/>
              {S.??("thank.you")}
            </p>
          </body>
        </html>)
    }

    def passwordResetEmailSubject = S.??("reset.password.request")

    def sendPasswordReset(e: String) {
        one(this where {email is_== e}) match {
            case Full(user) if user.validated =>
                user.resetUniqueId
                save(user)
                val resetLink = S.hostAndPath+
                passwordResetPath.mkString("/", "/", "/")+user.uniqueId

                val email: String = user.email

                val msgXml = passwordResetMailBody(user, resetLink)
                Mailer.sendMail(From(emailFrom),Subject(passwordResetEmailSubject),
                                (To(user.email) :: xmlToMailBodyType(msgXml) ::
                                (bccEmail.toList.map(BCC(_)))) :_*)

                S.notice(S.??("password.reset.email.sent"))
                S.redirectTo(homePage)

            case Full(user) =>
                sendValidationEmail(user)
                S.notice(S.??("account.validation.resent"))
                S.redirectTo(homePage)

            case _ => S.error(S.??("email.address.not.found"))
        }
    }

    def lostPassword = {
        bind("user", lostPasswordXhtml,
             "email" -> SHtml.text("", sendPasswordReset _),
             "submit" -> <input type="Submit" value={S.??("send.it")} />)
    }

    def passwordResetXhtml = {
        (<form method="post" action={S.uri}>
         <table><tr><td colspan="2">{S.??("reset.your.password")}</td></tr>
           <tr><td>{S.??("enter.your.new.password")}</td><td><user:pwd/></td></tr>
           <tr><td>{S.??("repeat.your.new.password")}</td><td><user:pwd/></td></tr>
           <tr><td>&nbsp;</td><td><user:submit/></td></tr>
         </table>
         </form>)
    }

    def passwordReset(id: String) =
        one(this where {uniqueId is_== id}) match {
            case Full(user) =>
                def finishSet() {
                    validate(user) match {
                        case Nil => S.notice(S.??("password.changed"))
                        save(user)
                        logUserIn(user); S.redirectTo(homePage)

                        case xs => S.error(xs)
                    }
                }
                user.resetUniqueId
                save(user)

                bind("user", passwordResetXhtml,
                     "pwd" -> SHtml.password_*("",(p: List[String]) => password.setFromAny(user, p)),
                     "submit" -> SHtml.submit(S.??("set.password"), finishSet _))
            case _ => S.error(S.??("pasword.link.invalid")); S.redirectTo(homePage)
        }

    def changePasswordXhtml = {
        (<form method="post" action={S.uri}>
            <table><tr><td colspan="2">{S.??("change.password")}</td></tr>
              <tr><td>{S.??("old.password")}</td><td><user:old_pwd /></td></tr>
              <tr><td>{S.??("new.password")}</td><td><user:new_pwd /></td></tr>
              <tr><td>{S.??("repeat.password")}</td><td><user:new_pwd /></td></tr>
              <tr><td>&nbsp;</td><td><user:submit /></td></tr>
            </table>
         </form>)
    }

    def changePassword = {
        val user = currentUser.open_! // we can do this because the logged in test has happened
        var oldPassword = ""
        var newPassword: List[String] = Nil

        def testAndSet() {
            if (!user.passwordMatch_?(oldPassword)) S.error(S.??("wrong.old.password"))
            else {
                password.setFromAny(user, newPassword)
                validate(user) match {
                    case Nil => save(user); S.notice(S.??("pasword.changed")); S.redirectTo(homePage)
                    case xs => S.error(xs)
                }
            }
        }

        bind("user", changePasswordXhtml,
             "old_pwd" -> SHtml.password("", oldPassword = _),
             "new_pwd" -> SHtml.password_*("", LFuncHolder(newPassword = _)),
             "submit" -> SHtml.submit(S.??("change"), testAndSet _))
    }

    def editXhtml(user: ModelType) = {
        (<form method="post" action={S.uri}>
            <table><tr><td colspan="2">{S.??("edit")}</td></tr>
              {localForm(user, true)}
              <tr><td>&nbsp;</td><td><user:submit/></td></tr>
            </table>
         </form>)
    }

    object editFunc extends RequestVar[Box[() => NodeSeq]](Empty)

    def edit = {
        val theUser: ModelType = currentUser.open_! // we know we're logged in
        val theName = editPath.mkString("")

        def testEdit() {
            validate(theUser) match {
                case Nil =>
                    save(theUser)
                    S.notice(S.??("profle.updated"))
                    S.redirectTo(homePage)

                case xs => S.error(xs) ; editFunc(Full(innerEdit _))
            }
        }

        def innerEdit = bind("user", editXhtml(theUser),
                             "submit" -> SHtml.submit(S.??("edit"), testEdit _))

        innerEdit
    }

    def logout = {
        logoutCurrentUser
        S.redirectTo(homePage)
    }

    private def localForm(user: ModelType, ignorePassword: Boolean): NodeSeq = {
        signupFields.
        filter { f => !ignorePassword || f.ne(password) }.
        flatMap { f =>
            f.toForm(user).toList map { form =>
                (<tr><td>{f.displayName}</td><td>{form}</td></tr>) } }
    }

    protected def wrapIt(in: NodeSeq): NodeSeq =
        screenWrap.map(new RuleTransformer(new RewriteRule {
            override def transform(n: Node) = n match {
                case e: Elem if "bind" == e.label && "lift" == e.prefix => in
                case _ => n
            }
        })) openOr in
}