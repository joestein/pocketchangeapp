package bootstrap.liftweb

import java.sql.{Connection, DriverManager}

import net.liftweb.common.{Box,Empty,Failure,Full,Logger}
import net.liftweb.util.{Helpers,LoanWrapper}
import net.liftweb.http.{GetRequest,LiftRules,ParsePath,PutRequest,Req,
                         RequestVar,RewriteRequest,RewriteResponse,S}
import net.liftweb.sitemap.{Loc,Menu,SiteMap}
import com.pocketchangeapp.model._
import com.pocketchangeapp.api._
import com.pocketchangeapp.util.{Charting,Image}
import com.pocketchangeapp.db.Database

// Get implicit conversions 
import net.liftweb.sitemap.Loc._
import net.liftweb.util.Helpers._

import com.pocketchangeapp.model._
import com.pocketchangeapp.api._
import com.pocketchangeapp.util.{Charting,Image}


 
/**
 * The bootstrap.liftweb.Boot class is the main entry point for Lift.
 * This is where all of the initial App setup is performed. In particular,
 * the Boot.boot method is what lift calls after it loads.
 * 
 * TODO: Connect Lucene/Compass for search
 */
class Boot {
  // Set up a logger to use for startup messages
  val logger = Logger(classOf[Boot])

  def boot {
    /*
     * LiftRules.early allows us to apply functions to the request before
     * Lift has started to work with it. In our case, we're explicitly
     * setting the character encoding to work around an issue with
     * Jetty not properly discovering the encoding from the client.
     */
    LiftRules.early.append {
      _.setCharacterEncoding("UTF-8")
    }

    Database.ensureIndexes()
    S.addAround(Database.inLiftRequest)
    LiftRules.addToPackages("com.pocketchangeapp")     

    LiftRules.setSiteMap(SiteMap(MenuInfo.menu :_*))

    // Tie in the REST API. Uncomment the one you want to use
    //LiftRules.dispatch.prepend(DispatchRestAPI.dispatch)
    LiftRules.dispatch.prepend(RestHelperAPI)

    // Set up some rewrites
    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("account", acctName), _, _, _), _, _) =>
	RewriteResponse("viewAcct" :: Nil, Map("name" -> urlDecode(acctName)))
      case RewriteRequest(ParsePath(List("account", acctName, tag), _, _, _), _, _) =>
	RewriteResponse("viewAcct" :: Nil, Map("name" -> urlDecode(acctName), "tag" -> urlDecode(tag)))
    }

    // Custom dispatch for graph and receipt image generation
    LiftRules.dispatch.append {
      case Req(List("graph", acctName, "history"), _, _) =>
	() => Charting.history(acctName)
      case Req(List("graph", acctName, "tagpie"), _, _) =>
	() => Charting.tagpie(acctName)
      case Req(List("graph", acctName, "tagbar"), _, _) =>
	() => Charting.tagbar(acctName)
      case Req(List("image", expenseId), _, _) =>
	() => Full(Image.viewImage(expenseId))
    }

    // Hook in our REST API auth
    LiftRules.httpAuthProtectedResource.append(DispatchRestAPI.protection)

    /* We're going to use HTTP Basic auth for REST, although
     * technically this allows for its use anywhere in the app. */
    import net.liftweb.http.auth.{AuthRole,HttpBasicAuthentication,userRoles}
    LiftRules.authentication = HttpBasicAuthentication("PocketChange") {
      case (userEmail, userPass, _) => {
        logger.debug("Authenticating: " + userEmail)
        (User where {User.email is_== userEmail} in User.getCollection).headOption map { user =>
          if (user.passwordMatch_?(userPass)) {
            logger.debug("Auth succeeded for " + userEmail)
            User.logUserIn(user)

            // Compute all of the user roles
            userRoles(user.editable.map(acct => AuthRole("editAcct:" + acct.id)) ++
                      user.allAccounts.map(acct => AuthRole("viewAcct:" + acct.id)))
            true
          } else {
            logger.warn("Auth failed for " + userEmail)
            false
          }
        } getOrElse false
      }
    }

    logger.info("Bootstrap up")
  }
}

object MenuInfo {
  import Loc._
  // Define a simple test clause that we can use for multiple menu items
  val IfLoggedIn = If(() => User.currentUser.isDefined, "You must be logged in")

  def menu: List[Menu] = 
    List[Menu](Menu.i("Home") / "index",
               Menu.i("Manage Accounts") / "manage" >> IfLoggedIn,
               Menu.i("Add Account") / "editAcct" >> Hidden >> IfLoggedIn,
               // The DSL currently doesn't support head match, so we go old-school
               Menu(Loc("viewAcct", List("viewAcct") -> true, "View Account", Hidden, IfLoggedIn)),
               Menu.i("Help") / "help" / "index") :::
    User.sitemap
  
}
