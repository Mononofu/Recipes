module Handler.Recipe
  (
    getRecipeR,
    postRecipeR
    )
where

import Import

ingredientsField :: Field sub Ingredient [IngredientId]
ingredientsField = Field
    { fieldParse = \ingredientNames _ -> do
        ingredients <- runDB $ selectList [IngredientName <-. ingredientNames] []
        return $ Right $ Just $ map (\(Entity key _) -> key :: Key Ingredient) ingredients
    , fieldView = \idAttr nameAttr _ eResult isReq -> [whamlet|
<input id=#{idAttr} name=#{nameAttr} type=password>
<div>Confirm:
<input id=#{idAttr}-confirm name=#{nameAttr} type=password>
|]
    , fieldEnctype = UrlEncoded
    }

entryForm :: Form Recipe
entryForm = renderDivs $ Recipe
    <$> areq   textField "Name" Nothing
    <*> areq   textField "Description" Nothing
    <*> areq   ingredientsField "Ingredients" Nothing

getRecipeR :: Handler RepHtml
getRecipeR = do
    -- Get the list of articles inside the database.
    ingredients <- runDB $ selectList [] [Desc IngredientName]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet).
    (ingredientWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "ingredients")

postRecipeR :: Handler RepHtml
postRecipeR = do
    ((res,_),_) <- runFormPost entryForm
    case res of
         FormSuccess recipe -> do
            _ <- runDB $ insert recipe
            setMessage $ toHtml $ (recipeName recipe) <> " created"
            redirect $ RecipeR
         _ -> do
            redirect $ RecipeR
